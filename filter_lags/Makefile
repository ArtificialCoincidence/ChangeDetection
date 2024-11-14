# Compiler and flags
CXX = nvcc

# Target executable
TARGET = main

# Source files
SRCS = main.cpp matrix.cu
# Object files
OBJS = $(SRCS:.cpp=.o)
OBJS := $(OBJS:.cu=.o)  # Replace .cu with .o for CUDA files

# Rule to build the final executable
$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(OBJS)

# Rule to build object files for .cpp files
%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Rule to build object files for .cu files
%.o: %.cu
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Clean up build files
clean:
	rm -f $(OBJS) $(TARGET)

