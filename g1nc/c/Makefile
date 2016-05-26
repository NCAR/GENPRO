CC = gcc
CFLAGS = -g -Wall
CXX = g++
CXXFLAGS = -g -Wall

C_TARGETS = convert hist

OBJS = gbytes.o

all: $(C_TARGETS) convert2

$(TARGETS): % : %.c
	$(CC) $(CFLAGS) $< -o $@

$(OBJS): %.o : %.cpp
	$(CXX) $(CXXFLAGS) -c $<

convert2 : % : %.cpp
	$(CXX) $(CXXFLAGS) $< -o $@
