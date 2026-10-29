# 🌽 Reski — Fortran High-Performance Web Server & Portfolio

Website portofolio pribadi dan mesin simulasi agrikultur murni 100% menggunakan bahasa pemrograman **Fortran (gfortran)** dengan low-level POSIX Sockets, tanpa framework pihak ketiga.

## 🚀 Fitur Utama

- **100% Native Fortran Web Server:** Menggunakan POSIX Socket ABI Linux langsung (`socket`, `bind`, `listen`, `accept`).
- **Simulasi Pertanian:** Algoritma pemodelan pertumbuhan jagung & estimasi panen dihitung secara native di memori dengan Fortran.
- **Dynamic SVG Rendering:** Menghasilkan grafik panen secara real-time langsung dari biner Fortran.
- **Docker-Ready:** Menggunakan Alpine Linux yang sangat ringan untuk deployment cloud (Koyeb, Render, VPS).
- **Modern UI:** Antarmuka responsif berbasis Tailwind CSS.

## 📁 Struktur Proyek

```text
website-pribadi/
├── src/                # Folder 1: Seluruh Source Code Fortran (.f90)
│   └── server.f90      # Web server, POSIX socket handler & engine simulasi
├── assets/             # Folder 2: Aset Statis & Render 3D Blender
│   ├── kebun_jagung.png
│   └── pohon_kebun.png
├── Dockerfile          # Konfigurasi container Docker Alpine + gfortran
├── Makefile            # Build automation untuk kompilasi biner
├── run.sh              # Skrip peluncur lokal (Port 8080)
├── start-cloudflare.sh # Skrip peluncur publik via Cloudflare Tunnel
└── index.html          # Template antarmuka web
```

## 🛠️ Cara Menjalankan Secara Lokal

```bash
# Kompilasi dan jalankan
make run

# Akses di browser:
# http://localhost:8080
```

## 🐳 Menjalankan via Docker

```bash
docker build -t fortran-web .
docker run -p 8080:8000 fortran-web
```

---
*Dibuat oleh **Rezky Eka Putra (Reski)**.*
