#!/bin/bash
# ==============================================================================
#  Peluncur Otomatis Server Fortran & Cloudflare Tunnel
#  RESKI — Bare-Bone Close-to-the-Metal Platform
# ==============================================================================
set -e
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$DIR"

echo "🌽 [1/3] Menyiapkan Server Web Fortran (Port 8080)..."

# Cek apakah server Fortran sudah berjalan di port 8080
if ! fuser 8080/tcp >/dev/null 2>&1; then
    if [ ! -f personal_site ] || [ src/server.f90 -nt personal_site ]; then
        echo "⚙️ Mengompilasi biner Fortran..."
        gfortran -O3 -march=native -pipe -fomit-frame-pointer -Wall src/server.f90 -o personal_site
    fi
    nohup ./personal_site < /dev/null > /tmp/fortran-server.log 2>&1 &
    disown || true
    sleep 1
fi

echo "☁️ [2/3] Membuka Terowongan Cloudflare (Cloudflare Tunnel)..."
pkill -f "cloudflared tunnel.*8080" 2>/dev/null || true
rm -f /tmp/cloudflared-fortran.log

nohup /home/reski/bin/cloudflared tunnel --url http://127.0.0.1:8080 < /dev/null > /tmp/cloudflared-fortran.log 2>&1 &
disown || true

echo "⏳ [3/3] Menghubungkan ke Edge Cloudflare Global..."
TUNNEL_URL=""
for i in {1..15}; do
    sleep 1
    if [ -f /tmp/cloudflared-fortran.log ]; then
        TUNNEL_URL=$(grep -o 'https://[-a-zA-Z0-9.]*\.trycloudflare\.com' /tmp/cloudflared-fortran.log | tail -n 1 || true)
        if [ -n "$TUNNEL_URL" ]; then
            break
        fi
    fi
done

echo ""
echo "=========================================================================="
echo "🎉 WEBSITE FORTRAN RESKI SUDAH AKTIF DI CLOUDFLARE!"
echo "=========================================================================="
echo "🏠 Akses Lokal:       http://localhost:8080"
if [ -n "$TUNNEL_URL" ]; then
    echo "🌐 Link Publik HTTPS: $TUNNEL_URL"
    echo "📊 API Simulasi:      $TUNNEL_URL/api/simulasi?lahan=1.0&pohon=500&hst=35"
    echo "📈 Grafik Panen:      $TUNNEL_URL/svg/yield.svg?target=8.5"
    echo "🖼️ Render 3D Kebun:   $TUNNEL_URL/assets/kebun_jagung.png"
else
    echo "⚠️ Belum mendapatkan URL, periksa log: cat /tmp/cloudflared-fortran.log"
fi
echo "=========================================================================="
echo "💡 Untuk mematikan server & tunnel, jalankan: ./stop-cloudflare.sh"
echo ""
