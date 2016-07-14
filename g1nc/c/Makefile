CC = gcc
CFLAGS = -g -Wall
CXX = g++
CXXFLAGS = -g -Wall

C_TARGETS = hist
CXX_TARGETS = genpro2nc

# Courtesy of http://stackoverflow.com/a/12368262
GIT_VERSION := $(shell git describe --abbrev=8 --dirty --always --tags)

LIBS = -lnetcdf

OBJS = rules.o

all: $(C_TARGETS) $(CXX_TARGETS)

$(C_TARGETS): % : %.c
	$(CC) $(CFLAGS) $< -o $@

$(CXX_TARGETS) : % : %.cpp $(OBJS)
	$(CXX) $(CXXFLAGS) $(OBJS) $(LIBS) $< -o $@

$(OBJS) : %.o : %.cpp
	$(CXX) $(CXXFLAGS) -DPROCESSOR_REVISION=\"$(GIT_VERSION)\" -c $<
