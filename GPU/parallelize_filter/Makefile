# Compiler and flags
CXX = nvcc
CXXFLAGS = -arch=sm_90 -g -G

# Target executable
TARGET = main

# Source files
SRCS = main.cu matrix.cu
# Object files
OBJS = $(SRCS:.cpp=.o)

# Rule to build the final executable
$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(OBJS)

# Rule to build object files
%.o: %.cu matrix.h
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Clean up build files
clean:
	rm -rf main
