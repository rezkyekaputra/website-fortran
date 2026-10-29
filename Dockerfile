# Base image super ringan (Alpine Linux)
FROM alpine:latest

# Install compiler GNU Fortran & C library
RUN apk add --no-cache gfortran musl-dev

# Set working directory
WORKDIR /app

# Copy seluruh file website, gambar assets, dan source code Fortran
COPY . .

# Compile source code Fortran menjadi biner mesin dengan optimasi tertinggi
RUN gfortran -O3 src/server.f90 -o personal_site

# Buka port 8000
EXPOSE 8000

# Jalankan server murni Fortran
CMD ["./personal_site"]
