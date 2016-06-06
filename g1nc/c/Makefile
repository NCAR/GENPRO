CC = gcc
CFLAGS = -g -Wall
CXX = g++
CXXFLAGS = -g -Wall

C_TARGETS = hist
CXX_TARGETS = genpro2nc

LIBS = -lnetcdf

OBJS = rules.o

all: $(C_TARGETS) $(CXX_TARGETS)

$(C_TARGETS): % : %.c
	$(CC) $(CFLAGS) $< -o $@

$(CXX_TARGETS) : % : %.cpp $(OBJS)
	$(CXX) $(CXXFLAGS) $(OBJS) $(LIBS) $< -o $@

$(OBJS) : %.o : %.cpp
	$(CXX) $(CXXFLAGS) -c $<
