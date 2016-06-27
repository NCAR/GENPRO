CC = gcc
CFLAGS = -g -Wall
CXX = g++
CXXFLAGS = -g -Wall

C_TARGETS = hist
CXX_TARGETS = genpro2nc

LIBS = -lnetcdf

all: $(C_TARGETS) $(CXX_TARGETS)

$(TARGETS): % : %.c
	$(CC) $(CFLAGS) $< -o $@

$(CXX_TARGETS) : % : %.cpp
	$(CXX) $(CXXFLAGS) $(LIBS) $< -o $@
