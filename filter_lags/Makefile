# Compiler and flags
CXX = nvcc

# Target executable
TARGET = main

# Source files
SRCS = main.cpp matrix.cpp
# Object files
OBJS = $(SRCS:.cpp=.o)

# Rule to build the final executable
$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(OBJS)

# Rule to build object files
%.o: %.cpp matrix.h
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Clean up build files
clean:
	rm -f $(OBJS) $(TARGET)

