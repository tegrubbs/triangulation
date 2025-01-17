LIBS= libeggx2003.a libeggx.a /usr/local/lib/libfortran_stdlib.a -lX11
include= -I/usr/local/include/fortran_stdlib/GNU-11.4.0/

all: main genGrid genCircle

main:
	gfortran -c  draw.f90 triangulate.f90 main.f90  $(include)
	gfortran -o main main.o draw.o triangulate.o  $(LIBS)
	rm *.o

genGrid:
	gfortran  -o genGrid genGrid.f90	

genCircle:
	gfortran -o genCircle genGridCircle.f90

clean:
	rm genGrid main genCircle
