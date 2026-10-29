! ==============================================================================
!  RESKI — BARE-BONE CLOSE-TO-THE-METAL FORTRAN PLATFORM
!  Pertanian Jagung Tropis, Profil Pribadi, dan Full Kapabilitas Fortran
!  Standar: Fortran 2008 / 2018 | Linux POSIX Sockets | Zero External Framework
! ==============================================================================

module posix_sockets
    use, intrinsic :: iso_c_binding
    implicit none

    type, bind(C) :: sockaddr_in
        integer(c_int16_t) :: sin_family
        integer(c_int16_t) :: sin_port
        integer(c_int32_t) :: sin_addr
        character(kind=c_char) :: sin_zero(8)
    end type sockaddr_in

    interface
        function c_socket(domain, type, protocol) bind(C, name="socket") result(fd)
            import :: c_int
            integer(c_int), value :: domain, type, protocol
            integer(c_int) :: fd
        end function

        function c_setsockopt(fd, level, optname, optval, optlen) bind(C, name="setsockopt") result(res)
            import :: c_int
            integer(c_int), value :: fd, level, optname, optlen
            integer(c_int), intent(in) :: optval
            integer(c_int) :: res
        end function

        function c_bind(fd, addr, len) bind(C, name="bind") result(res)
            import :: c_int, sockaddr_in
            integer(c_int), value :: fd, len
            type(sockaddr_in), intent(in) :: addr
            integer(c_int) :: res
        end function

        function c_listen(fd, backlog) bind(C, name="listen") result(res)
            import :: c_int
            integer(c_int), value :: fd, backlog
            integer(c_int) :: res
        end function

        function c_accept(fd, addr, addrlen) bind(C, name="accept") result(res)
            import :: c_int, c_ptr
            integer(c_int), value :: fd
            type(c_ptr), value :: addr, addrlen
            integer(c_int) :: res
        end function

        function c_read(fd, buf, count) bind(C, name="read") result(res)
            import :: c_int, c_size_t, c_char
            integer(c_int), value :: fd
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_size_t), value :: count
            integer(c_int) :: res
        end function

        function c_write(fd, buf, count) bind(C, name="write") result(res)
            import :: c_int, c_size_t, c_char
            integer(c_int), value :: fd
            character(kind=c_char), intent(in) :: buf(*)
            integer(c_size_t), value :: count
            integer(c_int) :: res
        end function

        function c_close(fd) bind(C, name="close") result(res)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: res
        end function

        function c_htons(hostshort) bind(C, name="htons") result(netshort)
            import :: c_int16_t
            integer(c_int16_t), value :: hostshort
            integer(c_int16_t) :: netshort
        end function

        function c_signal(sig, handler) bind(C, name="signal") result(res)
            import :: c_int, c_intptr_t
            integer(c_int), value :: sig
            integer(c_intptr_t), value :: handler
            integer(c_intptr_t) :: res
        end function
    end interface
end module posix_sockets


module file_streamer
    use, intrinsic :: iso_c_binding
    use posix_sockets
    implicit none
    character(len=2), parameter :: CRLF = char(13) // char(10)

contains

    subroutine send_all(fd, buf, total_len)
        integer(c_int), intent(in) :: fd
        character(len=*), intent(in) :: buf
        integer, intent(in) :: total_len
        integer :: sent, ret
        sent = 0
        do while (sent < total_len)
            ret = c_write(fd, buf(sent + 1:total_len), int(total_len - sent, c_size_t))
            if (ret <= 0) exit
            sent = sent + ret
        end do
    end subroutine send_all

    subroutine serve_static_file(client_fd, filepath, mime_type)
        integer(c_int), intent(in) :: client_fd
        character(len=*), intent(in) :: filepath, mime_type
        integer :: u, file_size, status, bytes_left, chunk_size, total_sent
        character(len=256) :: header_str
        character(kind=c_char), dimension(16384) :: chunk_buf

        open(newunit=u, file=filepath, access='stream', form='unformatted', status='old', iostat=status)
        if (status /= 0) then
            header_str = "HTTP/1.1 404 Not Found" // CRLF // "Content-Length: 0" // CRLF // CRLF
            status = c_write(client_fd, header_str, int(len_trim(header_str), c_size_t))
            return
        end if

        inquire(unit=u, size=file_size)

        write(header_str, '(A,A,A,I0,A)') &
            "HTTP/1.1 200 OK" // CRLF // &
            "Content-Type: ", trim(mime_type), CRLF // &
            "Cache-Control: public, max-age=86400" // CRLF // &
            "Connection: close" // CRLF // &
            "Content-Length: ", file_size, CRLF // CRLF

        status = c_write(client_fd, header_str, int(len_trim(header_str), c_size_t))
        if (status <= 0) then
            close(u)
            return
        end if

        bytes_left = file_size
        do while (bytes_left > 0)
            chunk_size = min(bytes_left, 16384)
            read(u) chunk_buf(1:chunk_size)

            total_sent = 0
            do while (total_sent < chunk_size)
                status = c_write(client_fd, chunk_buf(total_sent + 1), int(chunk_size - total_sent, c_size_t))
                if (status <= 0) exit
                total_sent = total_sent + status
            end do
            if (status <= 0) exit

            bytes_left = bytes_left - chunk_size
        end do

        close(u)
    end subroutine serve_static_file

end module file_streamer


module agro_corn_engine
    use, intrinsic :: iso_fortran_env, only: real64, int64
    implicit none
    public

    integer, parameter :: NX_PDE = 48
    integer, parameter :: NY_PDE = 14

contains

    subroutine calc_corn_phenology(hst, fase_code, fase_nama, fase_desc, &
                                   gdd_acc, tinggi_cm, air_mm_hari, biomassa_g)
        integer, intent(in) :: hst
        character(len=8), intent(out) :: fase_code
        character(len=32), intent(out) :: fase_nama
        character(len=128), intent(out) :: fase_desc
        real(real64), intent(out) :: gdd_acc, tinggi_cm, air_mm_hari, biomassa_g

        ! Akumulasi Growing Degree Days (GDD) rata-rata iklim tropis Indonesia (~14.5 C-day per hari)
        gdd_acc = real(hst, real64) * 14.5_real64

        if (hst <= 7) then
            fase_code = "VE"
            fase_nama = "Emergence (Perkecambahan)"
            fase_desc = "Koleoptil menembus permukaan tanah bedengan. Akar seminal mulai menyerap kelembaban tanah."
            tinggi_cm = max(1.0_real64, real(hst, real64) * 1.5_real64)
            air_mm_hari = 2.2_real64
            biomassa_g = 1.2_real64
        else if (hst <= 20) then
            fase_code = "V2 - V4"
            fase_nama = "Daun Awal & Akar Nodal"
            fase_desc = "Terbentuk 2-4 helai daun sejati dengan kerah daun (collar). Sistem perakaran nodal berkembang aktif."
            tinggi_cm = 10.0_real64 + real(hst - 7, real64) * 2.5_real64
            air_mm_hari = 3.5_real64
            biomassa_g = 6.5_real64
        else if (hst <= 40) then
            fase_code = "V6 - V8"
            fase_nama = "Vegetatif Cepat (Elongasi)"
            fase_desc = "Pertumbuhan eksponensial batang. Titik tumbuh di atas tanah, inisiasi pembentukan calon tongkol & malai jantan."
            tinggi_cm = 42.0_real64 + real(hst - 20, real64) * 5.8_real64
            air_mm_hari = 5.8_real64
            biomassa_g = 45.0_real64 + real(hst - 20, real64) * 5.5_real64
        else if (hst <= 60) then
            fase_code = "VT - R1"
            fase_nama = "Tasseling & Silking"
            fase_desc = "Fase teramat kritis! Malai jantan melepaskan serbuk sari, rambut tongkol (silk) menangkap polen. Kebutuhan air puncak."
            tinggi_cm = 158.0_real64 + real(hst - 40, real64) * 3.2_real64
            air_mm_hari = 7.8_real64
            biomassa_g = 155.0_real64 + real(hst - 40, real64) * 4.8_real64
        else if (hst <= 85) then
            fase_code = "R2 - R4"
            fase_nama = "Pengisian Biji (Blister-Dough)"
            fase_desc = "Biji jagung mengalami translokasi asimilat pati secara masif dari daun ke tongkol. Tekstur cairan menjadi pasta padat."
            tinggi_cm = 220.0_real64
            air_mm_hari = 5.2_real64
            biomassa_g = 250.0_real64 + real(hst - 60, real64) * 3.5_real64
        else
            fase_code = "R5 - R6"
            fase_nama = "Masak Fisiologis (Panen)"
            fase_desc = "Lapisan hitam (black layer) terbentuk di pangkal biji. Kadar air menurun ke ~20-25%. Jagung siap dipanen sempurna!"
            tinggi_cm = 220.0_real64
            air_mm_hari = 2.0_real64
            biomassa_g = 340.0_real64
        end if
    end subroutine calc_corn_phenology

    subroutine calc_fertilizer_and_economics(lahan_ha, jumlah_pohon, target_ton_ha, harga_kg, &
                                            populasi_total, urea_kg, sp36_kg, kcl_kg, &
                                            urea_g_pohon, sp36_g_pohon, kcl_g_pohon, &
                                            urea_dasar_kg, urea_sus1_kg, urea_sus2_kg, &
                                            omzet_rp, modal_rp, laba_rp, roi_pct)
        real(real64), intent(in) :: lahan_ha, target_ton_ha, harga_kg
        integer, intent(in) :: jumlah_pohon
        integer, intent(out) :: populasi_total
        real(real64), intent(out) :: urea_kg, sp36_kg, kcl_kg
        real(real64), intent(out) :: urea_g_pohon, sp36_g_pohon, kcl_g_pohon
        real(real64), intent(out) :: urea_dasar_kg, urea_sus1_kg, urea_sus2_kg
        real(real64), intent(out) :: omzet_rp, modal_rp, laba_rp, roi_pct

        real(real64) :: total_kg_panen, biaya_benih, biaya_pupuk, biaya_tenaga
        real(real64) :: skala_faktor

        ! Populasi ideal standar: Jarak tanam 75 cm x 20 cm = 66.666 tanaman/ha
        if (jumlah_pohon > 0) then
            populasi_total = jumlah_pohon
            skala_faktor = real(jumlah_pohon, real64) / 66666.0_real64
        else
            populasi_total = int(lahan_ha * 66666.0_real64)
            skala_faktor = lahan_ha
        end if

        ! Dosis anjuran per hektar untuk target hasil (Balitsereal standard calibration):
        ! Urea 46% N, SP-36 36% P2O5, KCl 60% K2O
        urea_kg = (target_ton_ha * 43.5_real64) * skala_faktor
        sp36_kg = (target_ton_ha * 18.5_real64) * skala_faktor
        kcl_kg  = (target_ton_ha * 13.0_real64) * skala_faktor

        ! Kebutuhan per individu pohon (gram/pohon):
        if (populasi_total > 0) then
            urea_g_pohon = (urea_kg * 1000.0_real64) / real(populasi_total, real64)
            sp36_g_pohon = (sp36_kg * 1000.0_real64) / real(populasi_total, real64)
            kcl_g_pohon  = (kcl_kg  * 1000.0_real64) / real(populasi_total, real64)
        else
            urea_g_pohon = 0.0_real64; sp36_g_pohon = 0.0_real64; kcl_g_pohon = 0.0_real64
        end if

        ! Jadwal Aplikasi 3 Tahap:
        ! 1. Pupuk Dasar (HST 7): 30% Urea, 100% SP-36, 30% KCl
        ! 2. Susulan I (HST 25): 40% Urea, 40% KCl
        ! 3. Susulan II (HST 45): 30% Urea, 30% KCl
        urea_dasar_kg = urea_kg * 0.30_real64
        urea_sus1_kg  = urea_kg * 0.40_real64
        urea_sus2_kg  = urea_kg * 0.30_real64

        ! Hasil Panen & Finansial
        total_kg_panen = (target_ton_ha * 1000.0_real64) * skala_faktor
        omzet_rp = total_kg_panen * harga_kg

        biaya_benih = 1400000.0_real64 * skala_faktor
        biaya_pupuk = (urea_kg * 4500.0_real64) + (sp36_kg * 5500.0_real64) + (kcl_kg * 8500.0_real64)
        biaya_tenaga = 4500000.0_real64 * skala_faktor
        modal_rp = biaya_benih + biaya_pupuk + biaya_tenaga

        laba_rp = omzet_rp - modal_rp
        if (modal_rp > 0.0_real64) then
            roi_pct = (laba_rp / modal_rp) * 100.0_real64
        else
            roi_pct = 0.0_real64
        end if
    end subroutine calc_fertilizer_and_economics

    ! 2D Finite Difference Soil Moisture Diffusion Solver (PDE)
    ! Fortran Array Slicing SIMD computation
    subroutine solve_soil_diffusion(steps, grid_ascii, elapsed_us)
        integer, intent(in) :: steps
        character(len=NX_PDE), dimension(NY_PDE), intent(out) :: grid_ascii
        real(real64), intent(out) :: elapsed_us

        real(real64), dimension(NX_PDE, NY_PDE) :: M1, M2
        real(real64) :: alpha, dist_furrow1, dist_furrow2, min_m, max_m, norm_m
        integer(int64) :: t1, t2, clock_rate
        integer :: s, i, j, g_idx
        character(len=1) :: glyphs(8)

        glyphs = [".", ":", "-", "=", "+", "*", "#", "@"]
        alpha = 0.22_real64 ! Stabilitas Von Neumann (alpha <= 0.25)

        call system_clock(count_rate=clock_rate)
        call system_clock(t1)

        ! Inisialisasi: Kelembaban latar belakang bedengan tanah (25% volume air)
        M1 = 25.0_real64

        ! Sumber infiltrasi parit air di kiri & kanan guludan
        do j = 2, NY_PDE - 1
            do i = 2, NX_PDE - 1
                dist_furrow1 = sqrt(real((i - 12)**2 + (j - 4)**2, real64))
                dist_furrow2 = sqrt(real((i - 36)**2 + (j - 4)**2, real64))
                if (dist_furrow1 < 6.0_real64) M1(i, j) = M1(i, j) + 45.0_real64 * exp(-0.15_real64 * dist_furrow1**2)
                if (dist_furrow2 < 6.0_real64) M1(i, j) = M1(i, j) + 45.0_real64 * exp(-0.15_real64 * dist_furrow2**2)
            end do
        end do

        ! Integrasi numerik persamaan difusi beda hingga (Finite Difference Time Domain)
        ! Menggunakan Fortran Modern Array Slicing (SIMD vectorizable)
        do s = 1, steps
            M2 = M1
            M2(2:NX_PDE-1, 2:NY_PDE-1) = M1(2:NX_PDE-1, 2:NY_PDE-1) + &
                alpha * (M1(3:NX_PDE, 2:NY_PDE-1) - 2.0_real64 * M1(2:NX_PDE-1, 2:NY_PDE-1) + M1(1:NX_PDE-2, 2:NY_PDE-1)) + &
                alpha * (M1(2:NX_PDE-1, 3:NY_PDE) - 2.0_real64 * M1(2:NX_PDE-1, 2:NY_PDE-1) + M1(2:NX_PDE-1, 1:NY_PDE-2))

            ! Serapan zona perakaran jagung aktif di pusat guludan (kedalaman sel 6 - 12)
            M2(20:28, 6:12) = M2(20:28, 6:12) - 0.08_real64
            where (M2 < 5.0_real64) M2 = 5.0_real64
            M1 = M2
        end do

        call system_clock(t2)
        elapsed_us = real(t2 - t1, real64) * 1.0d6 / real(clock_rate, real64)
        if (elapsed_us < 1.0_real64) elapsed_us = 1.0_real64

        min_m = minval(M1)
        max_m = maxval(M1)

        do j = 1, NY_PDE
            do i = 1, NX_PDE
                if (max_m > min_m) then
                    norm_m = (M1(i, j) - min_m) / (max_m - min_m)
                else
                    norm_m = 0.0_real64
                end if
                g_idx = max(1, min(8, int(norm_m * 7.0_real64) + 1))
                grid_ascii(j)(i:i) = glyphs(g_idx)
            end do
        end do
    end subroutine solve_soil_diffusion

    ! Fortran Intrinsic Linear Algebra & Matrix Math Benchmark
    subroutine benchmark_matrix_math(n_dim, elapsed_us, checksum)
        integer, intent(in) :: n_dim
        real(real64), intent(out) :: elapsed_us, checksum
        real(real64), dimension(:,:), allocatable :: A, B, C
        integer(int64) :: t1, t2, clock_rate
        integer :: i, j

        allocate(A(n_dim, n_dim), B(n_dim, n_dim), C(n_dim, n_dim))

        ! Inisialisasi matriks koefisien hara & penyerapan tanah
        do j = 1, n_dim
            do i = 1, n_dim
                A(i, j) = sin(real(i + j, real64) * 0.05_real64)
                B(i, j) = cos(real(i - j, real64) * 0.05_real64)
            end do
        end do

        call system_clock(count_rate=clock_rate)
        call system_clock(t1)

        ! Operasi matriks bawaan Fortran: matmul, transpose, dot_product, array reduction
        C = matmul(A, transpose(B))
        checksum = sum(C) + dot_product(A(:, 1), B(1, :))

        call system_clock(t2)
        elapsed_us = real(t2 - t1, real64) * 1.0d6 / real(clock_rate, real64)
        if (elapsed_us < 0.1_real64) elapsed_us = 0.1_real64

        deallocate(A, B, C)
    end subroutine benchmark_matrix_math

end module agro_corn_engine


module svg_render_engine
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    public

contains

    ! Kurva Respon Pemupukan Mitscherlich-Baule (NPK vs Hasil Panen Ton/Ha)
    subroutine generate_mitscherlich_svg(target_ton, out_svg, out_len)
        real(real64), intent(in) :: target_ton
        character(len=*), intent(out) :: out_svg
        integer, intent(out) :: out_len

        character(len=512) :: pt_buf
        real(real64) :: x_dose, y_yield, x_px, y_px
        integer :: i

        out_svg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 540 220" class="w-full">' // &
            '<defs><linearGradient id="neonG" x1="0" y1="0" x2="0" y2="1">' // &
            '<stop offset="0%" stop-color="#10b981" stop-opacity="0.35"/>' // &
            '<stop offset="100%" stop-color="#10b981" stop-opacity="0.0"/>' // &
            '</linearGradient></defs>' // &
            '<line x1="50" y1="30" x2="50" y2="180" stroke="#334155" stroke-width="1.2"/>' // &
            '<line x1="50" y1="180" x2="500" y2="180" stroke="#334155" stroke-width="1.2"/>' // &
            '<text x="42" y="35" fill="#94a3b8" font-size="10" text-anchor="end" font-family="monospace">12 T</text>' // &
            '<text x="42" y="85" fill="#94a3b8" font-size="10" text-anchor="end" font-family="monospace">8 T</text>' // &
            '<text x="42" y="135" fill="#94a3b8" font-size="10" text-anchor="end" font-family="monospace">4 T</text>' // &
            '<text x="42" y="184" fill="#94a3b8" font-size="10" text-anchor="end" font-family="monospace">0</text>' // &
            '<text x="500" y="200" fill="#94a3b8" font-size="10" text-anchor="end" font-family="monospace">Dosis Urea (Kg/Ha) &rarr;</text>' // &
            '<path d="M 50 175 '

        ! Kurva non-linear exponensial respon hasil panen
        do i = 1, 15
            x_dose = real(i - 1, real64) * 40.0_real64
            y_yield = 2.0_real64 + 9.5_real64 * (1.0_real64 - exp(-0.0075_real64 * x_dose))
            x_px = 50.0_real64 + (x_dose / 600.0_real64) * 450.0_real64
            y_px = 180.0_real64 - (y_yield / 12.0_real64) * 150.0_real64
            write(pt_buf, '(A,F6.1,A,F6.1)') ' L ', x_px, ' ', y_px
            out_svg = trim(out_svg) // trim(pt_buf)
        end do

        out_svg = trim(out_svg) // ' L 500 180 Z" fill="url(#neonG)"/>' // &
            '<path d="M 50 175 '

        do i = 1, 15
            x_dose = real(i - 1, real64) * 40.0_real64
            y_yield = 2.0_real64 + 9.5_real64 * (1.0_real64 - exp(-0.0075_real64 * x_dose))
            x_px = 50.0_real64 + (x_dose / 600.0_real64) * 450.0_real64
            y_px = 180.0_real64 - (y_yield / 12.0_real64) * 150.0_real64
            write(pt_buf, '(A,F6.1,A,F6.1)') ' L ', x_px, ' ', y_px
            out_svg = trim(out_svg) // trim(pt_buf)
        end do

        y_px = 180.0_real64 - (target_ton / 12.0_real64) * 150.0_real64
        x_px = 50.0_real64 + min(430.0_real64, max(40.0_real64, (target_ton / 11.5_real64) * 350.0_real64))

        write(pt_buf, '(A,F6.1,A,F6.1,A,F6.1,A,F6.1,A,F5.1,A)') &
            '" fill="none" stroke="#10b981" stroke-width="2.5"/>' // &
            '<circle cx="', x_px, '" cy="', y_px, '" r="5.5" fill="#f59e0b" stroke="#ffffff" stroke-width="1.8"/>' // &
            '<text x="', x_px + 10.0_real64, '" y="', y_px - 8.0_real64, &
            '" fill="#f59e0b" font-size="10" font-family="monospace" font-weight="bold">TARGET: ', target_ton, ' T/Ha</text></svg>'

        out_svg = trim(out_svg) // trim(pt_buf)
        out_len = len_trim(out_svg)
    end subroutine generate_mitscherlich_svg

    ! Kurva Dinamika Fenologi & Biomassa Jagung (HST vs Biomassa & Tinggi)
    subroutine generate_phenology_svg(cur_hst, out_svg, out_len)
        integer, intent(in) :: cur_hst
        character(len=*), intent(out) :: out_svg
        integer, intent(out) :: out_len

        character(len=512) :: pt_buf
        real(real64) :: h_val, b_val, x_px, y_px
        integer :: h

        out_svg = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 540 220" class="w-full">' // &
            '<line x1="50" y1="30" x2="50" y2="180" stroke="#334155" stroke-width="1.2"/>' // &
            '<line x1="50" y1="180" x2="500" y2="180" stroke="#334155" stroke-width="1.2"/>' // &
            '<text x="42" y="35" fill="#94a3b8" font-size="10" text-anchor="end" font-family="monospace">350g</text>' // &
            '<text x="42" y="85" fill="#94a3b8" font-size="10" text-anchor="end" font-family="monospace">200g</text>' // &
            '<text x="42" y="135" fill="#94a3b8" font-size="10" text-anchor="end" font-family="monospace">50g</text>' // &
            '<text x="42" y="184" fill="#94a3b8" font-size="10" text-anchor="end" font-family="monospace">0</text>' // &
            '<text x="500" y="200" fill="#94a3b8" font-size="10" text-anchor="end" font-family="monospace">Umur Tanaman (HST) &rarr;</text>' // &
            '<!-- Biomass sigmoidal curve -->' // &
            '<path d="M 50 180 '

        do h = 0, 110, 5
            h_val = real(h, real64)
            ! Kurva logistik biomassa jagung tropis (Sigmoid S-Curve)
            b_val = 350.0_real64 / (1.0_real64 + exp(-0.085_real64 * (h_val - 52.0_real64)))
            x_px = 50.0_real64 + (h_val / 110.0_real64) * 450.0_real64
            y_px = 180.0_real64 - (b_val / 350.0_real64) * 150.0_real64
            write(pt_buf, '(A,F6.1,A,F6.1)') ' L ', x_px, ' ', y_px
            out_svg = trim(out_svg) // trim(pt_buf)
        end do

        out_svg = trim(out_svg) // '" fill="none" stroke="#06b6d4" stroke-width="2.5"/>'

        ! Indikator posisi HST saat ini
        h_val = real(min(110, max(0, cur_hst)), real64)
        b_val = 350.0_real64 / (1.0_real64 + exp(-0.085_real64 * (h_val - 52.0_real64)))
        x_px = 50.0_real64 + (h_val / 110.0_real64) * 450.0_real64
        y_px = 180.0_real64 - (b_val / 350.0_real64) * 150.0_real64

        write(pt_buf, '(A,F6.1,A,F6.1,A,F6.1,A,F6.1,A,F6.1,A,F6.1,A,I0,A)') &
            '<line x1="', x_px, '" y1="30" x2="', x_px, '" y2="180" stroke="#f59e0b" stroke-dasharray="3,3" stroke-width="1.2"/>' // &
            '<circle cx="', x_px, '" cy="', y_px, '" r="5.5" fill="#f59e0b" stroke="#ffffff" stroke-width="1.8"/>' // &
            '<text x="', min(420.0_real64, x_px + 8.0_real64), '" y="', y_px - 8.0_real64, &
            '" fill="#f59e0b" font-size="10" font-family="monospace" font-weight="bold">HST ', cur_hst, '</text></svg>'

        out_svg = trim(out_svg) // trim(pt_buf)
        out_len = len_trim(out_svg)
    end subroutine generate_phenology_svg

end module svg_render_engine


module http_query_parser
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    public

contains

    subroutine parse_query_real(req, key, default_val, result_val)
        character(len=*), intent(in) :: req, key
        real(real64), intent(in) :: default_val
        real(real64), intent(out) :: result_val
        integer :: k_idx, j, stat
        character(len=32) :: buf

        result_val = default_val
        k_idx = index(req, trim(key))
        if (k_idx > 0) then
            buf = req(k_idx + len_trim(key) : min(len(req), k_idx + len_trim(key) + 20))
            do j = 1, len(buf)
                if (buf(j:j) == '&' .or. buf(j:j) == ' ' .or. buf(j:j) == char(13) .or. buf(j:j) == char(10)) then
                    buf(j:) = ' '
                    exit
                end if
            end do
            read(buf, *, iostat=stat) result_val
            if (stat /= 0) result_val = default_val
        end if
    end subroutine parse_query_real

    subroutine parse_query_int(req, key, default_val, result_val)
        character(len=*), intent(in) :: req, key
        integer, intent(in) :: default_val
        integer, intent(out) :: result_val
        integer :: k_idx, j, stat
        character(len=32) :: buf

        result_val = default_val
        k_idx = index(req, trim(key))
        if (k_idx > 0) then
            buf = req(k_idx + len_trim(key) : min(len(req), k_idx + len_trim(key) + 20))
            do j = 1, len(buf)
                if (buf(j:j) == '&' .or. buf(j:j) == ' ' .or. buf(j:j) == char(13) .or. buf(j:j) == char(10)) then
                    buf(j:) = ' '
                    exit
                end if
            end do
            read(buf, *, iostat=stat) result_val
            if (stat /= 0) result_val = default_val
        end if
    end subroutine parse_query_int

end module http_query_parser


program fortran_barebone_platform
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use posix_sockets
    use file_streamer
    use agro_corn_engine
    use svg_render_engine
    use http_query_parser
    implicit none

    integer(c_int) :: server_fd, client_fd, status, optval
    type(sockaddr_in) :: addr
    character(kind=c_char) :: req_buffer(4096)
    character(len=4096) :: req_str
    character(len=262144), save :: html_doc
    character(len=16384), save  :: svg_buf, json_buf
    character(len=512)    :: header_str
    character(len=16)     :: env_port
    character(len=64)     :: p_buf1, p_buf2
    character(len=NX_PDE), dimension(NY_PDE) :: pde_grid
    character(len=8)      :: fase_code
    character(len=32)     :: fase_nama
    character(len=128)    :: fase_desc

    real(real64) :: lahan_ha, target_ton_ha, harga_kg
    real(real64) :: gdd_acc, tinggi_cm, air_mm_hari, biomassa_g
    real(real64) :: urea_kg, sp36_kg, kcl_kg
    real(real64) :: urea_g_pohon, sp36_g_pohon, kcl_g_pohon
    real(real64) :: urea_dasar_kg, urea_sus1_kg, urea_sus2_kg
    real(real64) :: omzet_rp, modal_rp, laba_rp, roi_pct
    real(real64) :: pde_us, mat_us, mat_checksum
    real(real64) :: req_us
    integer(int64) :: t_req_start, t_req_end, clock_rate

    integer :: jumlah_pohon, populasi_total, hst
    integer :: bytes_read, body_len, header_len, idx, svg_len, j
    integer :: port_num = 8080
    integer(c_intptr_t) :: sig_res

    ! Abaikan SIGPIPE (13) agar disconnect dari browser tidak mematikan server
    sig_res = c_signal(13_c_int, 1_c_intptr_t)

    server_fd = c_socket(2_c_int, 1_c_int, 0_c_int)
    if (server_fd < 0) stop "Socket creation failed"

    optval = 1_c_int
    status = c_setsockopt(server_fd, 1_c_int, 2_c_int, optval, 4_c_int)

    call get_environment_variable("PORT", env_port, status=status)
    if (status == 0) then
        read(env_port, *, iostat=status) port_num
        if (port_num <= 0 .or. port_num > 65535) port_num = 8080
    end if

    addr%sin_family = 2_c_int16_t
    addr%sin_port = c_htons(int(port_num, c_int16_t))
    addr%sin_addr = 0_c_int32_t
    addr%sin_zero = c_null_char

    status = c_bind(server_fd, addr, int(sizeof(addr), c_int))
    if (status /= 0) then
        print *, "Error binding to port", port_num, ". Trying fallback port 8000..."
        port_num = 8000
        addr%sin_port = c_htons(int(port_num, c_int16_t))
        status = c_bind(server_fd, addr, int(sizeof(addr), c_int))
        if (status /= 0) stop "Failed to bind socket"
    end if

    status = c_listen(server_fd, 25_c_int)
    if (status /= 0) stop "Failed to listen"

    print *, ""
    print *, "=========================================================================="
    print *, " 🌽 RESKI — BARE-BONE CLOSE-TO-THE-METAL FORTRAN WEB PLATFORM"
    print *, " Pertanian Jagung Tropis • Profil Diri • Full Kemampuan Fortran 2008"
    print *, " Port: ", port_num
    print *, " URL : http://localhost:", port_num
    print *, " Hardware: AMD Puma (btver2) x86_64 | Zero External Web Framework"
    print *, "=========================================================================="
    print *, ""

    do
        client_fd = c_accept(server_fd, c_null_ptr, c_null_ptr)
        if (client_fd < 0) cycle

        call system_clock(count_rate=clock_rate)
        call system_clock(t_req_start)

        req_buffer = c_null_char
        bytes_read = c_read(client_fd, req_buffer, 4095_c_size_t)
        if (bytes_read <= 0) then
            status = c_close(client_fd)
            cycle
        end if

        req_str = ""
        do idx = 1, min(bytes_read, 4095)
            req_str(idx:idx) = req_buffer(idx)
        end do

        ! Favicon handler
        if (index(req_str, "/favicon.ico") > 0) then
            header_str = "HTTP/1.1 204 No Content" // CRLF // "Connection: close" // CRLF // CRLF
            status = c_write(client_fd, header_str, int(len_trim(header_str), c_size_t))
            status = c_close(client_fd)
            cycle
        end if

        ! Binary Asset Streaming: Karya Asli Render 3D Blender Reski
        if (index(req_str, "/assets/kebun_jagung.png") > 0) then
            call serve_static_file(client_fd, "assets/kebun_jagung.png", "image/png")
            status = c_close(client_fd)
            cycle
        end if

        if (index(req_str, "/assets/pohon_kebun.png") > 0) then
            call serve_static_file(client_fd, "assets/pohon_kebun.png", "image/png")
            status = c_close(client_fd)
            cycle
        end if

        ! Parse Query Parameters
        call parse_query_real(req_str, "lahan=", 1.0_real64, lahan_ha)
        if (lahan_ha < 0.01_real64) lahan_ha = 0.01_real64
        if (lahan_ha > 100.0_real64) lahan_ha = 100.0_real64

        call parse_query_int(req_str, "pohon=", 0, jumlah_pohon)
        if (jumlah_pohon < 0) jumlah_pohon = 0
        if (jumlah_pohon > 5000000) jumlah_pohon = 5000000

        call parse_query_int(req_str, "hst=", 45, hst)
        if (hst < 1) hst = 1
        if (hst > 120) hst = 120

        call parse_query_real(req_str, "target=", 8.5_real64, target_ton_ha)
        if (target_ton_ha < 1.0_real64) target_ton_ha = 1.0_real64
        if (target_ton_ha > 14.0_real64) target_ton_ha = 14.0_real64

        call parse_query_real(req_str, "harga=", 5200.0_real64, harga_kg)
        if (harga_kg < 1000.0_real64) harga_kg = 1000.0_real64

        ! Eksekusi Komputasi Agronomi di Fortran
        call calc_corn_phenology(hst, fase_code, fase_nama, fase_desc, &
                                 gdd_acc, tinggi_cm, air_mm_hari, biomassa_g)

        call calc_fertilizer_and_economics(lahan_ha, jumlah_pohon, target_ton_ha, harga_kg, &
                                           populasi_total, urea_kg, sp36_kg, kcl_kg, &
                                           urea_g_pohon, sp36_g_pohon, kcl_g_pohon, &
                                           urea_dasar_kg, urea_sus1_kg, urea_sus2_kg, &
                                           omzet_rp, modal_rp, laba_rp, roi_pct)

        ! Route: Dynamic SVG Mitscherlich Yield Curve
        if (index(req_str, "GET /svg/yield.svg") > 0) then
            call generate_mitscherlich_svg(target_ton_ha, svg_buf, svg_len)
            write(header_str, '(A,I0,A)') &
                "HTTP/1.1 200 OK" // CRLF // &
                "Content-Type: image/svg+xml; charset=utf-8" // CRLF // &
                "Cache-Control: no-cache" // CRLF // &
                "Connection: close" // CRLF // &
                "Content-Length: ", svg_len, CRLF // CRLF
            status = c_write(client_fd, header_str, int(len_trim(header_str), c_size_t))
            call send_all(client_fd, svg_buf, svg_len)
            status = c_close(client_fd)
            cycle
        end if

        ! Route: Dynamic SVG Growth & Phenology Curve
        if (index(req_str, "GET /svg/growth.svg") > 0) then
            call generate_phenology_svg(hst, svg_buf, svg_len)
            write(header_str, '(A,I0,A)') &
                "HTTP/1.1 200 OK" // CRLF // &
                "Content-Type: image/svg+xml; charset=utf-8" // CRLF // &
                "Cache-Control: no-cache" // CRLF // &
                "Connection: close" // CRLF // &
                "Content-Length: ", svg_len, CRLF // CRLF
            status = c_write(client_fd, header_str, int(len_trim(header_str), c_size_t))
            call send_all(client_fd, svg_buf, svg_len)
            status = c_close(client_fd)
            cycle
        end if

        ! Route: REST JSON API /api/simulasi
        if (index(req_str, "GET /api/simulasi") > 0) then
            write(json_buf, '(A,F5.2,A,I0,A,I0,A,F5.2,A,A,A,A,A,A,F7.1,A,F6.1,A,F5.1,A,F6.1,A,F7.2,A,F7.2,A,F7.2,A,F6.2,A,F6.2,A,F6.2,A,F12.0,A,F12.0,A,F12.0,A,F6.1,A)') &
                '{"engine":"FORTRAN 2008 NATIVE","lahan_ha":', lahan_ha, &
                ',"populasi":', populasi_total, &
                ',"hst":', hst, &
                ',"target_ton":', target_ton_ha, &
                ',"fase_code":"', trim(fase_code), '","fase_nama":"', trim(fase_nama), '"', &
                ',"gdd_acc":', gdd_acc, &
                ',"tinggi_cm":', tinggi_cm, &
                ',"air_mm_hari":', air_mm_hari, &
                ',"biomassa_g":', biomassa_g, &
                ',"urea_kg":', urea_kg, ',"sp36_kg":', sp36_kg, ',"kcl_kg":', kcl_kg, &
                ',"urea_g_pohon":', urea_g_pohon, ',"sp36_g_pohon":', sp36_g_pohon, ',"kcl_g_pohon":', kcl_g_pohon, &
                ',"omzet_rp":', omzet_rp, ',"modal_rp":', modal_rp, ',"laba_rp":', laba_rp, &
                ',"roi_pct":', roi_pct, '}'
            body_len = len_trim(json_buf)
            write(header_str, '(A,I0,A)') &
                "HTTP/1.1 200 OK" // CRLF // &
                "Content-Type: application/json; charset=utf-8" // CRLF // &
                "Access-Control-Allow-Origin: *" // CRLF // &
                "Connection: close" // CRLF // &
                "Content-Length: ", body_len, CRLF // CRLF
            status = c_write(client_fd, header_str, int(len_trim(header_str), c_size_t))
            call send_all(client_fd, json_buf, body_len)
            status = c_close(client_fd)
            cycle
        end if

        ! Route: REST JSON API /api/matrix (Benchmark Matriks Fortran)
        if (index(req_str, "GET /api/matrix") > 0) then
            call benchmark_matrix_math(120, mat_us, mat_checksum)
            write(json_buf, '(A,F8.2,A,F14.2,A)') &
                '{"engine":"FORTRAN MATMUL & TRANSPOSE","matrix_dim":"120x120","elapsed_us":', mat_us, &
                ',"checksum":', mat_checksum, '}'
            body_len = len_trim(json_buf)
            write(header_str, '(A,I0,A)') &
                "HTTP/1.1 200 OK" // CRLF // &
                "Content-Type: application/json; charset=utf-8" // CRLF // &
                "Access-Control-Allow-Origin: *" // CRLF // &
                "Connection: close" // CRLF // &
                "Content-Length: ", body_len, CRLF // CRLF
            status = c_write(client_fd, header_str, int(len_trim(header_str), c_size_t))
            call send_all(client_fd, json_buf, body_len)
            status = c_close(client_fd)
            cycle
        end if

        ! Eksekusi Solusi Beda Hingga Difusi Air Tanah 2D
        call solve_soil_diffusion(200, pde_grid, pde_us)

        ! Eksekusi Benchmark Aljabar Linier Fortran
        call benchmark_matrix_math(100, mat_us, mat_checksum)

        ! Hitung Waktu Proses Fortran sampai saat ini
        call system_clock(t_req_end)
        req_us = real(t_req_end - t_req_start, real64) * 1.0d6 / real(clock_rate, real64)
        if (req_us < 1.0_real64) req_us = 1.0_real64

        ! ======================================================================
        ! GENERASI HALAMAN HTML MURNI (EMBEDDED CSS • ZERO CDN • CLOSE-TO-METAL)
        ! ======================================================================
        html_doc = '<!DOCTYPE html><html lang="id"><head><meta charset="UTF-8">' // &
            '<meta name="viewport" content="width=device-width, initial-scale=1.0">' // &
            '<title>RESKI — Pure Fortran Corn Agronomy & Bare-Bone Engine</title>' // &
            '<style>' // &
            ':root { --bg:#07080b; --card:#0f1118; --card-hover:#141720; --border:#1e2333; ' // &
            '--text:#f8fafc; --muted:#94a3b8; --green:#10b981; --gold:#f59e0b; --cyan:#06b6d4; --font-mono:ui-monospace,"JetBrains Mono",monospace; }' // &
            '* { box-sizing:border-box; margin:0; padding:0; }' // &
            'body { background-color:var(--bg); color:var(--text); font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,sans-serif; line-height:1.55; padding-bottom:60px; -webkit-font-smoothing:antialiased; }' // &
            '.mono { font-family:var(--font-mono); }' // &
            '.container { max-width:1120px; margin:0 auto; padding:0 20px; }' // &
            'header { border-bottom:1px solid var(--border); padding:16px 0; background:rgba(7,8,11,0.92); position:sticky; top:0; z-index:100; backdrop-filter:blur(8px); }' // &
            '.nav-wrap { display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:12px; }' // &
            '.brand { display:flex; align-items:center; gap:12px; text-decoration:none; color:inherit; }' // &
            '.brand-badge { background:linear-gradient(135deg,var(--green),var(--cyan)); color:#000; font-weight:900; padding:4px 10px; border-radius:8px; font-size:14px; }' // &
            '.nav-links { display:flex; gap:18px; font-size:13px; font-weight:500; }' // &
            '.nav-links a { color:var(--muted); text-decoration:none; transition:color 0.2s; }' // &
            '.nav-links a:hover { color:var(--green); }' // &
            '.hud-pill { display:inline-flex; align-items:center; gap:8px; padding:4px 12px; border-radius:20px; background:#041c14; border:1px solid #065f46; font-size:11px; color:var(--green); }' // &
            '.dot { width:8px; height:8px; border-radius:50%; background:var(--green); box-shadow:0 0 8px var(--green); }' // &
            'section { margin-top:40px; }' // &
            '.card { background:var(--card); border:1px solid var(--border); border-radius:14px; padding:24px; transition:border-color 0.2s; }' // &
            '.card:hover { border-color:#2a3449; }' // &
            'h1 { font-size:2.2rem; font-weight:900; line-height:1.2; letter-spacing:-0.03em; margin-bottom:14px; }' // &
            'h2 { font-size:1.4rem; font-weight:800; letter-spacing:-0.02em; margin-bottom:10px; color:#fff; }' // &
            'h3 { font-size:1.1rem; font-weight:700; color:#e2e8f0; margin-bottom:6px; }' // &
            'p { color:var(--muted); font-size:14px; line-height:1.6; margin-bottom:12px; }' // &
            '.tag { display:inline-block; font-size:10px; font-family:var(--font-mono); font-weight:700; text-transform:uppercase; padding:2px 8px; border-radius:4px; margin-bottom:8px; }' // &
            '.tag-green { background:rgba(16,185,129,0.15); color:var(--green); border:1px solid rgba(16,185,129,0.3); }' // &
            '.tag-gold { background:rgba(245,158,11,0.15); color:var(--gold); border:1px solid rgba(245,158,11,0.3); }' // &
            '.tag-cyan { background:rgba(6,182,212,0.15); color:var(--cyan); border:1px solid rgba(6,182,212,0.3); }' // &
            '.grid-2 { display:grid; grid-template-columns:1fr; gap:20px; }' // &
            '@media(min-width:768px){ .grid-2 { grid-template-columns:1fr 1fr; } }' // &
            '.grid-3 { display:grid; grid-template-columns:1fr; gap:16px; }' // &
            '@media(min-width:640px){ .grid-3 { grid-template-columns:repeat(3, 1fr); } }' // &
            '.grid-4 { display:grid; grid-template-columns:1fr 1fr; gap:14px; }' // &
            '@media(min-width:768px){ .grid-4 { grid-template-columns:repeat(4, 1fr); } }' // &
            '.stat-box { background:#080a0f; border:1px solid var(--border); border-radius:10px; padding:16px; }' // &
            '.stat-val { font-size:1.45rem; font-weight:800; font-family:var(--font-mono); margin-top:4px; }' // &
            '.stat-lbl { font-size:11px; color:var(--muted); font-family:var(--font-mono); text-transform:uppercase; }' // &
            'form { display:grid; grid-template-columns:1fr; gap:14px; }' // &
            '@media(min-width:768px){ form { grid-template-columns:repeat(4, 1fr); } }' // &
            'label { display:block; font-size:11px; font-family:var(--font-mono); text-transform:uppercase; color:var(--muted); margin-bottom:6px; font-weight:600; }' // &
            'input, select { width:100%; background:#05070a; border:1px solid var(--border); border-radius:8px; padding:10px 12px; color:#fff; font-size:13px; outline:none; font-family:var(--font-mono); }' // &
            'input:focus, select:focus { border-color:var(--green); }' // &
            '.btn { background:var(--green); color:#000; font-weight:800; border:none; border-radius:8px; padding:10px 18px; font-size:13px; font-family:var(--font-mono); cursor:pointer; text-transform:uppercase; letter-spacing:0.04em; transition:background 0.2s; }' // &
            '.btn:hover { background:#34d399; }' // &
            'pre { background:#040608; border:1px solid var(--border); border-radius:10px; padding:16px; font-size:11px; line-height:1.35; overflow-x:auto; color:var(--cyan); }' // &
            '.img-wrap { aspect-ratio:16/9; background:#040608; border-radius:10px; overflow:hidden; border:1px solid var(--border); margin-bottom:12px; }' // &
            '.img-wrap img { width:100%; height:100%; object-fit:cover; display:block; }' // &
            '.spec-item { padding:14px; border-radius:8px; background:#090c12; border:1px solid var(--border); }' // &
            '.spec-title { font-size:13px; font-weight:700; color:#fff; margin-bottom:4px; display:flex; align-items:center; gap:6px; }' // &
            '.spec-desc { font-size:12px; color:var(--muted); line-height:1.45; }' // &
            '.code-snippet { background:#000; padding:6px 8px; border-radius:4px; color:var(--green); font-size:11px; display:inline-block; margin-top:6px; border:1px solid #14281f; }' // &
            'table { width:100%; border-collapse:collapse; font-size:12px; text-align:left; font-family:var(--font-mono); }' // &
            'th { border-bottom:1px solid var(--border); padding:8px; color:var(--muted); font-size:11px; }' // &
            'td { padding:10px 8px; border-bottom:1px solid #141720; }' // &
            'footer { border-top:1px solid var(--border); margin-top:60px; padding:30px 0; font-size:12px; color:var(--muted); font-family:var(--font-mono); text-align:center; }' // &
            '</style></head><body>' // new_line('a')

        ! Header
        html_doc = trim(html_doc) // &
            '<header><div class="container nav-wrap">' // &
            '<a href="/" class="brand">' // &
            '<span class="brand-badge">F77/08</span>' // &
            '<div><div style="font-weight:800;font-size:15px;letter-spacing:-0.02em;">RESKI</div>' // &
            '<div class="mono" style="font-size:10px;color:var(--muted);">BARE-BONE FORTRAN SYSTEM</div></div></a>' // &
            '<nav class="nav-links">' // &
            '<a href="#profil">Profil Diri</a>' // &
            '<a href="#agronomi">Kalkulator Jagung</a>' // &
            '<a href="#karya3d">Render 3D</a>' // &
            '<a href="#fortran-power">Full Fortran</a>' // &
            '<a href="#pde-mesh">2D Infiltrasi PDE</a>' // &
            '<a href="#kontak">Kontak</a>' // &
            '</nav>' // &
            '<div class="hud-pill mono"><span class="dot"></span>' // &
            '<span>CPU LATENCY: '

        write(p_buf1, '(F8.1)') req_us
        html_doc = trim(html_doc) // trim(adjustl(p_buf1)) // ' &#956;s</span></div></div></header>' // new_line('a')

        ! Main Container
        html_doc = trim(html_doc) // '<main class="container">' // new_line('a')

        ! Hero Section
        html_doc = trim(html_doc) // &
            '<section style="margin-top:30px;">' // &
            '<div class="tag tag-green">100% PURE FORTRAN 2008 &bull; LINUX POSIX SOCKETS &bull; ZERO NPM / NO FRAMEWORK</div>' // &
            '<h1>Agronomi Jagung Tropis &amp; Komputasi Tingkat Rendah <span style="color:var(--green);">(Close to the Metal)</span></h1>' // &
            '<p style="max-width:850px;font-size:15px;">' // &
            'Platform web mandiri yang ditulis murni dalam <b>Fortran 2008</b> dan dikompilasi langsung ke biner native x86_64 Linux. ' // &
            'Menghubungkan sains budidaya tanaman jagung berimbang, perancangan spasial 3D Blender, dan superkomputasi numerik langsung di register prosesor tanpa lapisan perantara.' // &
            '</p>' // &
            '<div class="grid-4" style="margin-top:20px;">' // &
            '<div class="stat-box"><div class="stat-lbl">Runtime Dependencies</div><div class="stat-val" style="color:var(--green);">0 Libs</div></div>' // &
            '<div class="stat-box"><div class="stat-lbl">Fortran Exec Time</div><div class="stat-val" style="color:var(--cyan);">' // &
            trim(adjustl(p_buf1)) // ' &#956;s</div></div>' // &
            '<div class="stat-box"><div class="stat-lbl">Memory Footprint</div><div class="stat-val" style="color:var(--gold);">&lt; 3.2 MB</div></div>' // &
            '<div class="stat-box"><div class="stat-lbl">Compiler Suite</div><div class="stat-val" style="color:#fff;">GCC 15.2</div></div>' // &
            '</div></section>' // new_line('a')

        ! Section: Profil Diri Reski
        html_doc = trim(html_doc) // &
            '<section id="profil" class="card">' // &
            '<div class="tag tag-cyan">01 // TENTANG PENGEMBANG</div>' // &
            '<h2>Profil Diri &amp; Filosofi Rekayasa Reski</h2>' // &
            '<div class="grid-2" style="margin-top:14px;align-items:start;">' // &
            '<div>' // &
            '<p>Halo! Saya <b>Reski</b>. Aktivitas harian saya bergerak di antara dua spektrum yang sering dianggap berseberangan: <b>tanah basah bedengan kebun jagung</b> dan <b>arsitektur mikroprosesor sistem operasi Linux</b>.</p>' // &
            '<p>Di lapangan, saya mendalami agronomi tanaman jagung hibrida (<i>Zea mays L.</i>)—mulai dari mekanika aerasi guludan, kinetika serapan hara N-P-K bertahap, dinamika evapotranspirasi kanopi, hingga optimalisasi hasil panen tonase tinggi.</p>' // &
            '<p>Di laboratorium komputer, saya adalah penganut prinsip <b>close to the metal</b> dan filosofi <i>suckless</i>: perangkat lunak harus efisien, deterministik, dan menghormati setiap siklus clock CPU. Saya membangun server ini langsung di atas Gentoo Linux menggunakan compiler Fortran tanpa runtime virtual machine atau ribuan dependensi eksternal.</p>' // &
            '</div>' // &
            '<div class="spec-item" style="background:#05070a;">' // &
            '<div class="mono" style="font-size:12px;color:var(--gold);margin-bottom:10px;font-weight:700;">// TOOLCHAIN &amp; PREFERENSI KERJA</div>' // &
            '<ul style="list-style:none;font-size:13px;line-height:1.9;color:var(--muted);">' // &
            '<li>🌾 <b>Agronomi:</b> Presisi Dosis Jagung, Pola Tanam Guludan, Estimasi ROI.</li>' // &
            '<li>⚡ <b>Bahasa:</b> Modern Fortran (2008/2018), ISO_C_BINDING, C, Shell Script.</li>' // &
            '<li>🧊 <b>Visualisasi:</b> Blender 4.4 Cycles Rendering, Model Morfologi Daun.</li>' // &
            '<li>🐧 <b>Sistem Operasi:</b> Gentoo Linux, Custom Kernel, AMD Puma btver2.</li>' // &
            '<li>📐 <b>Matematika:</b> Aljabar Linier BLAS/LAPACK, Finite Difference FDM 2D.</li>' // &
            '</ul></div></div></section>' // new_line('a')

        ! Section: Interactive Corn Agronomy Lab
        html_doc = trim(html_doc) // &
            '<section id="agronomi" class="card">' // &
            '<div class="tag tag-gold">02 // LABORATORIUM AGRONOMI INTERAKTIF</div>' // &
            '<h2>Kalkulator Presisi Jagung &amp; Kebutuhan Hara Berimbang</h2>' // &
            '<p>Kalkulasi biologi tanaman dan neraca hara dihitung secara real-time oleh modul Fortran. Anda dapat menghitung skala hektar penuh maupun skala populasi pohon tertentu (misal: 500 pohon sesuai catatan lapangan).</p>' // &
            '<form method="GET" action="/#agronomi" style="margin-top:18px;">' // &
            '<div><label>Luas Lahan (Hektar)</label>' // &
            '<select name="lahan">'

        if (abs(lahan_ha - 0.25_real64) < 0.05_real64) then
            html_doc = trim(html_doc) // '<option value="0.25" selected>0.25 Ha (2.500 m&#178;)</option>'
        else
            html_doc = trim(html_doc) // '<option value="0.25">0.25 Ha (2.500 m&#178;)</option>'
        end if

        if (abs(lahan_ha - 0.5_real64) < 0.05_real64) then
            html_doc = trim(html_doc) // '<option value="0.5" selected>0.5 Ha (5.000 m&#178;)</option>'
        else
            html_doc = trim(html_doc) // '<option value="0.5">0.5 Ha (5.000 m&#178;)</option>'
        end if

        if (abs(lahan_ha - 1.0_real64) < 0.05_real64) then
            html_doc = trim(html_doc) // '<option value="1.0" selected>1.0 Hektar (Standar)</option>'
        else
            html_doc = trim(html_doc) // '<option value="1.0">1.0 Hektar (Standar)</option>'
        end if

        if (abs(lahan_ha - 2.0_real64) < 0.05_real64) then
            html_doc = trim(html_doc) // '<option value="2.0" selected>2.0 Hektar</option>'
        else
            html_doc = trim(html_doc) // '<option value="2.0">2.0 Hektar</option>'
        end if

        html_doc = trim(html_doc) // '</select></div>' // &
            '<div><label>Jumlah Pohon (Opsi Khusus)</label>'

        write(p_buf1, '(I0)') jumlah_pohon
        html_doc = trim(html_doc) // '<input type="number" name="pohon" min="0" max="500000" placeholder="0 = Gunakan Luas Ha" value="' // &
            trim(adjustl(p_buf1)) // '"></div>' // &
            '<div><label>Umur Tanaman (HST)</label>'

        write(p_buf1, '(I0)') hst
        html_doc = trim(html_doc) // '<input type="number" name="hst" min="1" max="115" value="' // &
            trim(adjustl(p_buf1)) // '"></div>' // &
            '<div><label>Target Hasil (Ton/Ha)</label>' // &
            '<select name="target">'

        if (abs(target_ton_ha - 6.0_real64) < 0.2_real64) then
            html_doc = trim(html_doc) // '<option value="6.0" selected>6.0 Ton/Ha</option>'
        else
            html_doc = trim(html_doc) // '<option value="6.0">6.0 Ton/Ha</option>'
        end if

        if (abs(target_ton_ha - 8.5_real64) < 0.2_real64) then
            html_doc = trim(html_doc) // '<option value="8.5" selected>8.5 Ton/Ha (Optimum)</option>'
        else
            html_doc = trim(html_doc) // '<option value="8.5">8.5 Ton/Ha (Optimum)</option>'
        end if

        if (abs(target_ton_ha - 11.0_real64) < 0.2_real64) then
            html_doc = trim(html_doc) // '<option value="11.0" selected>11.0 Ton/Ha (Intensif Tinggi)</option>'
        else
            html_doc = trim(html_doc) // '<option value="11.0">11.0 Ton/Ha (Intensif Tinggi)</option>'
        end if

        html_doc = trim(html_doc) // '</select></div>' // &
            '<div style="grid-column:1/-1;display:flex;justify-content:flex-end;">' // &
            '<button type="submit" class="btn">&#9889; Hitung Ulang dengan Fortran</button></div></form>' // new_line('a')

        ! Hasil Komputasi Fenologi
        html_doc = trim(html_doc) // &
            '<div style="margin-top:24px;border-top:1px solid var(--border);padding-top:20px;">' // &
            '<div class="grid-4">' // &
            '<div class="stat-box"><div class="stat-lbl">Fase Fenologi (HST '

        write(p_buf1, '(I0)') hst
        html_doc = trim(html_doc) // trim(adjustl(p_buf1)) // ')</div>' // &
            '<div class="stat-val" style="color:var(--gold);font-size:1.15rem;">' // trim(fase_code) // '</div>' // &
            '<div style="font-size:11px;color:#cbd5e1;margin-top:2px;">' // trim(fase_nama) // '</div></div>'

        write(p_buf1, '(F6.1)') gdd_acc
        html_doc = trim(html_doc) // &
            '<div class="stat-box"><div class="stat-lbl">Akumulasi GDD</div>' // &
            '<div class="stat-val" style="color:var(--cyan);">' // trim(adjustl(p_buf1)) // ' &deg;C-d</div>' // &
            '<div style="font-size:11px;color:var(--muted);margin-top:2px;">Tbase = 10 &deg;C</div></div>'

        write(p_buf1, '(F5.1)') tinggi_cm
        html_doc = trim(html_doc) // &
            '<div class="stat-box"><div class="stat-lbl">Tinggi Tajuk Tanaman</div>' // &
            '<div class="stat-val" style="color:var(--green);">' // trim(adjustl(p_buf1)) // ' cm</div>' // &
            '<div style="font-size:11px;color:var(--muted);margin-top:2px;">Morfologi Kanopi</div></div>'

        write(p_buf1, '(F5.1)') air_mm_hari
        html_doc = trim(html_doc) // &
            '<div class="stat-box"><div class="stat-lbl">Kebutuhan Air Harian</div>' // &
            '<div class="stat-val" style="color:#a855f7;">' // trim(adjustl(p_buf1)) // ' mm/d</div>' // &
            '<div style="font-size:11px;color:var(--muted);margin-top:2px;">Evapotranspirasi ETc</div></div></div>' // new_line('a')

        ! Penjelasan Fase Biologi
        html_doc = trim(html_doc) // &
            '<div style="background:#090d14;border:1px solid #1e283d;border-radius:8px;padding:12px 16px;margin-top:14px;font-size:12px;color:#94a3b8;">' // &
            '<b style="color:#fff;">Kondisi Fisiologis:</b> ' // trim(fase_desc) // '</div></div>' // new_line('a')

        ! Tabel Kebutuhan Pupuk & Jadwal Aplikasi
        html_doc = trim(html_doc) // &
            '<div style="margin-top:24px;">' // &
            '<h3>Rekomendasi Dosis Pupuk Berimbang &amp; Jadwal Aplikasi</h3>' // &
            '<div style="overflow-x:auto;margin-top:10px;">' // &
            '<table><thead><tr>' // &
            '<th>Jenis Pupuk</th><th>Kandungan Hara</th><th>Total Kebutuhan</th><th>Takaran Per Pohon</th><th>Jadwal Aplikasi Utama</th>' // &
            '</tr></thead><tbody>'

        ! Baris Urea
        write(p_buf1, '(F7.1)') urea_kg
        write(p_buf2, '(F5.2)') urea_g_pohon
        html_doc = trim(html_doc) // &
            '<tr><td><b style="color:#fff;">Urea</b></td><td>46% Nitrogen (N)</td>' // &
            '<td style="color:var(--green);font-weight:700;">' // trim(adjustl(p_buf1)) // ' kg</td>' // &
            '<td style="color:var(--gold);font-weight:700;">' // trim(adjustl(p_buf2)) // ' gram/pohon</td>' // &
            '<td>30% Dasar (HST 7) &bull; 40% Susulan I (HST 25) &bull; 30% Susulan II (HST 45)</td></tr>'

        ! Baris SP-36
        write(p_buf1, '(F7.1)') sp36_kg
        write(p_buf2, '(F5.2)') sp36_g_pohon
        html_doc = trim(html_doc) // &
            '<tr><td><b style="color:#fff;">SP-36</b></td><td>36% P<sub>2</sub>O<sub>5</sub> (Fosfat)</td>' // &
            '<td style="color:var(--green);font-weight:700;">' // trim(adjustl(p_buf1)) // ' kg</td>' // &
            '<td style="color:var(--gold);font-weight:700;">' // trim(adjustl(p_buf2)) // ' gram/pohon</td>' // &
            '<td>100% Diberikan saat Pupuk Dasar (HST 7) di samping lubang benih</td></tr>'

        ! Baris KCl
        write(p_buf1, '(F7.1)') kcl_kg
        write(p_buf2, '(F5.2)') kcl_g_pohon
        html_doc = trim(html_doc) // &
            '<tr><td><b style="color:#fff;">KCl</b></td><td>60% K<sub>2</sub>O (Kalium)</td>' // &
            '<td style="color:var(--green);font-weight:700;">' // trim(adjustl(p_buf1)) // ' kg</td>' // &
            '<td style="color:var(--gold);font-weight:700;">' // trim(adjustl(p_buf2)) // ' gram/pohon</td>' // &
            '<td>30% Dasar &bull; 40% Susulan I &bull; 30% Susulan II (Pengisian Biji)</td></tr>' // &
            '</tbody></table></div></div>' // new_line('a')

        ! Grafik Dinamis On-the-fly dari Fortran (Mitscherlich & Pertumbuhan)
        html_doc = trim(html_doc) // &
            '<div class="grid-2" style="margin-top:24px;">' // &
            '<div class="stat-box">' // &
            '<div style="display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;">' // &
            '<div class="stat-lbl">Kurva Respon Mitscherlich (NPK vs Hasil)</div>' // &
            '<span class="mono" style="font-size:10px;color:var(--green);">/svg/yield.svg</span></div>'

        write(p_buf1, '(F5.1)') target_ton_ha
        html_doc = trim(html_doc) // &
            '<img src="/svg/yield.svg?target=' // trim(adjustl(p_buf1)) // '" alt="Kurva Respon Hasil Panen" style="width:100%;height:auto;display:block;">' // &
            '<div style="font-size:11px;color:var(--muted);margin-top:6px;">Dihitung berdasarkan hukum kenaikan hasil yang semakin berkurang (Law of Diminishing Returns).</div></div>' // &
            '<div class="stat-box">' // &
            '<div style="display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;">' // &
            '<div class="stat-lbl">Dinamika Akumulasi Biomassa &amp; HST</div>' // &
            '<span class="mono" style="font-size:10px;color:var(--cyan);">/svg/growth.svg</span></div>'

        write(p_buf1, '(I0)') hst
        html_doc = trim(html_doc) // &
            '<img src="/svg/growth.svg?hst=' // trim(adjustl(p_buf1)) // '" alt="Kurva Pertumbuhan HST" style="width:100%;height:auto;display:block;">' // &
            '<div style="font-size:11px;color:var(--muted);margin-top:6px;">Kurva sigmoid akumulasi bahan kering dari fase vegetatif hingga masak fisiologis.</div></div></div>' // new_line('a')

        ! Analisis Kelayakan Finansial
        html_doc = trim(html_doc) // &
            '<div style="margin-top:24px;border-top:1px solid var(--border);padding-top:20px;">' // &
            '<h3>Analisis Finansial Usahatani (Populasi: '

        write(p_buf1, '(I0)') populasi_total
        html_doc = trim(html_doc) // trim(adjustl(p_buf1)) // ' Tanaman)</h3>' // &
            '<div class="grid-4" style="margin-top:12px;">'

        write(p_buf1, '(F10.2)') omzet_rp / 1.0d6
        html_doc = trim(html_doc) // &
            '<div class="stat-box"><div class="stat-lbl">Proyeksi Omzet</div>' // &
            '<div class="stat-val" style="color:#fff;">Rp ' // trim(adjustl(p_buf1)) // ' Jt</div></div>'

        write(p_buf1, '(F10.2)') modal_rp / 1.0d6
        html_doc = trim(html_doc) // &
            '<div class="stat-box"><div class="stat-lbl">Total Biaya Modal</div>' // &
            '<div class="stat-val" style="color:var(--muted);">Rp ' // trim(adjustl(p_buf1)) // ' Jt</div></div>'

        write(p_buf1, '(F10.2)') laba_rp / 1.0d6
        html_doc = trim(html_doc) // &
            '<div class="stat-box"><div class="stat-lbl">Laba Bersih Estimasi</div>' // &
            '<div class="stat-val" style="color:var(--green);">Rp ' // trim(adjustl(p_buf1)) // ' Jt</div></div>'

        write(p_buf1, '(F5.1)') roi_pct
        html_doc = trim(html_doc) // &
            '<div class="stat-box"><div class="stat-lbl">Return on Investment</div>' // &
            '<div class="stat-val" style="color:var(--cyan);">+' // trim(adjustl(p_buf1)) // ' %</div></div></div></div></section>' // new_line('a')

        ! Section: Render 3D Blender (Binary Streamed by Fortran)
        html_doc = trim(html_doc) // &
            '<section id="karya3d" class="card">' // &
            '<div class="tag tag-green">03 // KARYA BLENDER 4.4 &bull; BINARY STREAMING DARI FORTRAN</div>' // &
            '<h2>Visualisasi 3D Kebun Jagung &amp; Vegetasi Alami</h2>' // &
            '<p>Gambar di bawah dibaca langsung dari disk storage dalam blok biner 16 KB dan dikirimkan oleh socket TCP biner Fortran murni tanpa web server pihak ketiga.</p>' // &
            '<div class="grid-2" style="margin-top:16px;">' // &
            '<div class="stat-box">' // &
            '<div class="img-wrap"><img src="/assets/kebun_jagung.png" alt="3D Kebun Jagung Reski" loading="lazy"></div>' // &
            '<h3>Visualisasi 3D Kebun Jagung &amp; Bedengan</h3>' // &
            '<p>Pemodelan geometri guludan tanah, jarak tanam teratur, morfologi tajuk jagung, dan pencahayaan matahari langsung menggunakan mesin render Cycles.</p>' // &
            '<div class="mono" style="font-size:11px;color:var(--green);">Stream Source: /assets/kebun_jagung.png (655 KB)</div></div>' // &
            '<div class="stat-box">' // &
            '<div class="img-wrap"><img src="/assets/pohon_kebun.png" alt="3D Pohon Kebun Reski" loading="lazy"></div>' // &
            '<h3>Modeling Kanopi Pohon &amp; Vegetasi Kebun</h3>' // &
            '<p>Eksplorasi pembentukan ranting fraktal prosedural, kanopi daun rimbun sebagai peneduh batas lahan pertanian, dan tekstur batang alami.</p>' // &
            '<div class="mono" style="font-size:11px;color:var(--green);">Stream Source: /assets/pohon_kebun.png (399 KB)</div></div></div></section>' // new_line('a')

        ! Section: Full Capabilities of Fortran
        html_doc = trim(html_doc) // &
            '<section id="fortran-power" class="card">' // &
            '<div class="tag tag-cyan">04 // ARSENAL TEKNOLOGI</div>' // &
            '<h2>Full Apa yang Bisa Fortran Lakukan di Web &amp; Agrikultur</h2>' // &
            '<p>Fortran (Formula Translation) yang dirancang pertama kali pada 1957 oleh John Backus di IBM tetap menjadi standar tertinggi komputasi performa tinggi di dunia superkomputer hingga saat ini. Berikut kapabilitas penuh Fortran yang bekerja aktif pada server ini:</p>' // &
            '<div class="grid-2" style="margin-top:18px;">'

        ! Pilar 1
        html_doc = trim(html_doc) // &
            '<div class="spec-item"><div class="spec-title"><span>&#9889;</span> 1. Direct POSIX C Syscalls via ISO_C_BINDING</div>' // &
            '<div class="spec-desc">Fortran berinteraksi langsung dengan Application Binary Interface (ABI) kernel Linux tanpa wrapper. Membuka socket, binding port, listen, accept, dan streaming data network dengan zero overhead.</div>' // &
            '<div class="code-snippet">function c_socket(...) bind(C, name="socket")</div></div>'

        ! Pilar 2
        html_doc = trim(html_doc) // &
            '<div class="spec-item"><div class="spec-title"><span>&#128290;</span> 2. Native Multi-Dimensional Array Slicing</div>' // &
            '<div class="spec-desc">Operasi array tingkat tinggi yang otomatis di-vektorisasi oleh compiler ke register AVX/SSE prosesor tanpa perlunya perulangan manual (SIMD auto-vectorization).</div>' // &
            '<div class="code-snippet">tanah(2:NX-1, 2:NY-1) = tanah(...) * alpha</div></div>'

        ! Pilar 3
        write(p_buf1, '(F8.1)') mat_us
        html_doc = trim(html_doc) // &
            '<div class="spec-item"><div class="spec-title"><span>&#128397;</span> 3. Intrinsic Linear Algebra (MATMUL &amp; TRANSPOSE)</div>' // &
            '<div class="spec-desc">Fungsi bawaan aljabar linier teroptimasi untuk perkalian matriks koefisien hara tanah. ' // &
            'Benchmark real-time: Matriks 100x100 diselesaikan dalam <b>' // trim(adjustl(p_buf1)) // ' &#956;s</b> di CPU.</div>' // &
            '<div class="code-snippet">C = matmul(A, transpose(B))</div></div>'

        ! Pilar 4
        write(p_buf1, '(F8.1)') pde_us
        html_doc = trim(html_doc) // &
            '<div class="spec-item"><div class="spec-title"><span>&#127777;</span> 4. Solusi Numerik Beda Hingga (2D PDE FDM)</div>' // &
            '<div class="spec-desc">Menyelesaikan persamaan diferensial parsial difusi air dan dinamika infiltrasi kelembaban tanah bedengan 200 time-steps dalam <b>' // &
            trim(adjustl(p_buf1)) // ' &#956;s</b>.</div>' // &
            '<div class="code-snippet">call solve_soil_diffusion(200, grid, t)</div></div>'

        ! Pilar 5
        html_doc = trim(html_doc) // &
            '<div class="spec-item"><div class="spec-title"><span>&#9678;</span> 5. Presisi Ganda Floating Point (IEEE 754 REAL64)</div>' // &
            '<div class="spec-desc">Akurasi numerik 53-bit mantissa hingga 15-17 digit desimal signifikan, krusial untuk simulasi fenologi dan tidak rentan floating-point roundoff drift.</div>' // &
            '<div class="code-snippet">use, intrinsic :: iso_fortran_env, only: real64</div></div>'

        ! Pilar 6
        html_doc = trim(html_doc) // &
            '<div class="spec-item"><div class="spec-title"><span>&#127912;</span> 6. On-The-Fly Procedural Vector SVG Engine</div>' // &
            '<div class="spec-desc">Fortran langsung menghasilkan kode kurva vektor SVG secara matematis di memori tanpa dependensi library grafik eksternal apa pun.</div>' // &
            '<div class="code-snippet">write(out_svg, ...) // generate_mitscherlich_svg()</div></div>'

        ! Pilar 7
        html_doc = trim(html_doc) // &
            '<div class="spec-item"><div class="spec-title"><span>&#9201;</span> 7. Telemetri Hardware Siklus CPU (SYSTEM_CLOCK)</div>' // &
            '<div class="spec-desc">Mengakses counter clock hardware beresolusi mikrodetik untuk benchmarking latensi setiap paket HTTP yang masuk.</div>' // &
            '<div class="code-snippet">call system_clock(count_rate=rate, count=t1)</div></div>'

        ! Pilar 8
        html_doc = trim(html_doc) // &
            '<div class="spec-item"><div class="spec-title"><span>&#128080;</span> 8. In-Memory HTTP Query String Parser</div>' // &
            '<div class="spec-desc">Mekanisme parsing URL parameter dinamis murni string Fortran untuk menangani input form interaktif dari pengunjung secara instan.</div>' // &
            '<div class="code-snippet">call parse_query_real(req, "lahan=", 1.0, val)</div></div>'

        ! Pilar 9
        html_doc = trim(html_doc) // &
            '<div class="spec-item"><div class="spec-title"><span>&#128225;</span> 9. RESTful JSON API Serialization</div>' // &
            '<div class="spec-desc">Fortran memproduksi payload JSON standar pada endpoint <code>/api/simulasi</code> dan <code>/api/matrix</code> untuk integrasi dengan aplikasi eksternal.</div>' // &
            '<div class="code-snippet">write(json_buf, &apos;{"engine":"FORTRAN 2008", ...}&apos;)</div></div>'

        ! Pilar 10
        html_doc = trim(html_doc) // &
            '<div class="spec-item"><div class="spec-title"><span>&#128640;</span> 10. Zero-Memory-Leak Binary Chunk Streaming</div>' // &
            '<div class="spec-desc">Membaca file biner dari drive penyimpanan dalam buffer 16 KB dan mengirimkannya langsung ke socket TCP tanpa lonjakan konsumsi memori.</div>' // &
            '<div class="code-snippet">call serve_static_file(fd, path, mime)</div></div></div></section>' // new_line('a')

        ! Section: 2D Soil Moisture PDE Visualizer
        html_doc = trim(html_doc) // &
            '<section id="pde-mesh" class="card">' // &
            '<div class="tag tag-cyan">05 // SUPERKOMPUTASI PDE DIFUSI TANAH</div>' // &
            '<h2>Simulasi Numerik 2D Profil Kelembaban Bedengan Jagung</h2>' // &
            '<p>Hasil komputasi 200 iterasi beda hingga (FDM) difusi kelembaban air tanah di sekitar perakaran jagung aktif (Mesh 48 &times; 14 sel):</p>' // &
            '<div style="margin-top:10px;">' // &
            '<div style="display:flex;justify-content:space-between;font-size:11px;margin-bottom:6px;color:var(--cyan);" class="mono">' // &
            '<span>GRID MESH: 48 &times; 14 &bull; 200 TIME STEPS &bull; STABILITAS VON NEUMANN</span>' // &
            '<span>Dihitung dalam '

        write(p_buf1, '(F8.1)') pde_us
        html_doc = trim(html_doc) // trim(adjustl(p_buf1)) // ' &#956;s</span></div>' // &
            '<pre>'

        do j = 1, NY_PDE
            html_doc = trim(html_doc) // pde_grid(j) // new_line('a')
        end do

        html_doc = trim(html_doc) // '</pre>' // &
            '<div style="font-size:11px;color:var(--muted);margin-top:8px;" class="mono">' // &
            'Legenda Kerapatan Air: [ . Kering | : Lembab Rendah | - Sedang | = Lembab Aktif | + Kapasitas Lapang | * Basah | # Jenuh Parit | @ Titik Infiltrasi ]' // &
            '</div></div></section>' // new_line('a')

        ! Section: REST API Playground
        html_doc = trim(html_doc) // &
            '<section class="card">' // &
            '<div class="tag tag-gold">06 // REST API SERVICE</div>' // &
            '<h2>Akses Langsung via Terminal (cURL / HTTP)</h2>' // &
            '<p>Anda dapat menguji kecepatan respons server Fortran ini langsung dari terminal Anda:</p>' // &
            '<div style="margin-top:12px;display:flex;flex-direction:column;gap:10px;">' // &
            '<div class="spec-item"><div class="spec-title mono" style="color:var(--green);">cURL JSON Agronomi:</div>' // &
            '<div class="mono" style="font-size:12px;color:#cbd5e1;background:#040608;padding:8px 12px;border-radius:6px;overflow-x:auto;">'

        write(p_buf1, '(I0)') port_num
        html_doc = trim(html_doc) // &
            'curl -s &quot;http://localhost:' // trim(adjustl(p_buf1)) // '/api/simulasi?lahan=1.0&amp;pohon=66000&amp;hst=45&amp;target=8.5&amp;harga=5200&quot; | jq .' // &
            '</div></div>' // &
            '<div class="spec-item"><div class="spec-title mono" style="color:var(--cyan);">cURL Benchmark Matriks:</div>' // &
            '<div class="mono" style="font-size:12px;color:#cbd5e1;background:#040608;padding:8px 12px;border-radius:6px;overflow-x:auto;">' // &
            'curl -s &quot;http://localhost:' // trim(adjustl(p_buf1)) // '/api/matrix&quot;' // &
            '</div></div></div></section>' // new_line('a')

        ! Section: Kontak & Kolaborasi
        html_doc = trim(html_doc) // &
            '<section id="kontak" class="card" style="text-align:center;padding:36px 20px;">' // &
            '<div class="tag tag-green">07 // HUBUNGI SAYA</div>' // &
            '<h2>Mari Terhubung &amp; Berkolaborasi</h2>' // &
            '<p style="max-width:600px;margin:0 auto 20px auto;">' // &
            'Tertarik mendiskusikan implementasi budidaya jagung presisi, pemodelan visual 3D lanskap di Blender, atau efisiensi komputasi numerik Fortran di Linux? Silakan hubungi saya.' // &
            '</p>' // &
            '<div style="display:flex;justify-content:center;gap:14px;flex-wrap:wrap;">' // &
            '<a href="mailto:reski@example.com" class="btn" style="text-decoration:none;">Kirim Email</a>' // &
            '<a href="https://wa.me/" target="_blank" class="btn" style="background:#090d14;border:1px solid var(--border);color:#fff;text-decoration:none;">WhatsApp</a>' // &
            '</div></section>' // new_line('a')

        ! Footer
        html_doc = trim(html_doc) // '</main>' // &
            '<footer><div class="container">' // &
            '&copy; 2026 Reski &bull; 100% Pure Fortran 2008 &bull; Standalone POSIX Web Platform &bull; AMD Puma btver2 x86_64' // &
            '</div></footer></body></html>'

        body_len = len_trim(html_doc)

        write(header_str, '(A,I0,A)') &
            "HTTP/1.1 200 OK" // CRLF // &
            "Content-Type: text/html; charset=utf-8" // CRLF // &
            "Connection: close" // CRLF // &
            "Content-Length: ", body_len, CRLF // CRLF

        header_len = len_trim(header_str)

        status = c_write(client_fd, header_str, int(header_len, c_size_t))
        call send_all(client_fd, html_doc, body_len)
        status = c_close(client_fd)
    end do

    status = c_close(server_fd)
end program fortran_barebone_platform
