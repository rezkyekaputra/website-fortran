# ==============================================================================
#  RESKI — FORTRAN BARE-BONE CLOSE-TO-THE-METAL PLATFORM
#  Target CPU: AMD Puma (btver2) / Native x86_64 Linux
# ==============================================================================

FC = gfortran
FFLAGS = -O3 -march=native -pipe -fomit-frame-pointer -Wall
SRC = src/server.f90
BIN = personal_site
PORT ?= 8080

.PHONY: all clean run test

all: $(BIN)

$(BIN): $(SRC)
	@echo "==> Mengompilasi Fortran Standalone Binary..."
	$(FC) $(FFLAGS) $(SRC) -o $(BIN)
	@echo "==> Kompilasi selesai: $(BIN) ($(shell ls -lh $(BIN) | awk '{print $$5}'))"

run: $(BIN)
	@echo "==> Menjalankan Fortran Server pada port $(PORT)..."
	PORT=$(PORT) ./$(BIN)

test:
	@echo "==> Menjalankan Health Check pada http://localhost:$(PORT)..."
	@curl -s -I http://127.0.0.1:$(PORT)/ | head -n 5
	@echo ""
	@curl -s "http://127.0.0.1:$(PORT)/api/simulasi?lahan=1.0&pohon=500&hst=35&target=8.0&harga=5000"
	@echo ""

clean:
	@rm -f $(BIN) *.mod
	@echo "==> Clean selesai."
