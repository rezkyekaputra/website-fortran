#!/bin/bash
# ==============================================================================
# Peluncur Server Web Fortran Reski (Port 8080)
# ==============================================================================
set -e
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$DIR"

# Matikan proses lama jika ada di port 8080
fuser -k 8080/tcp 2>/dev/null || true

# Recompile jika biner belum ada atau src/server.f90 lebih baru
if [ ! -f personal_site ] || [ src/server.f90 -nt personal_site ]; then
    echo "⚙️ Mengompilasi src/server.f90..."
    gfortran -O3 -march=native -pipe -fomit-frame-pointer -Wall src/server.f90 -o personal_site
fi

echo "🚀 Menjalankan Fortran Server pada http://localhost:8080..."
exec ./personal_site
