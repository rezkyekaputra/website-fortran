#!/bin/bash
# ==============================================================================
#  Mematikan Server Fortran dan Cloudflare Tunnel
# ==============================================================================
echo "🛑 Menghentikan Cloudflare Tunnel..."
pkill -f "cloudflared tunnel.*8080" 2>/dev/null || true

echo "🛑 Menghentikan Server Fortran (Port 8080)..."
fuser -k 8080/tcp 2>/dev/null || true
pkill -f "personal_site" 2>/dev/null || true

echo "✅ Server Fortran dan Cloudflare Tunnel berhasil dinonaktifkan."
