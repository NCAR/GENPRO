CC = gcc
CFLAGS = -g -Wall
CXX = g++
CXXFLAGS = -g -Wall

C_TARGETS = hist
CXX_TARGETS = convert convert2

OBJS = gbytes.o

all: $(C_TARGETS) $(CXX_TARGETS)

$(TARGETS): % : %.c
	$(CC) $(CFLAGS) $< -o $@

$(OBJS): %.o : %.cpp
	$(CXX) $(CXXFLAGS) -c $<

$(CXX_TARGETS) : % : %.cpp
	$(CXX) $(CXXFLAGS) $< -o $@
