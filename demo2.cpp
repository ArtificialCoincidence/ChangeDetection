#include"matrix.h"
using namespace std;
using namespace std::chrono;

int main() {

	auto t1 = high_resolution_clock::now();
	Matrix testMatrix(500,500);
	Matrix refMatrix(500, 500);
	//Modify the file path if the file is moved.
	testMatrix.readData("C:/Users/chang/Desktop/Demos/SampleData/test0/Itest0.dat");
	refMatrix.readData("C:/Users/chang/Desktop/Demos/SampleData/test0/Iref0A.dat");

	auto t2 = high_resolution_clock::now();
	duration<double> duration = t2 - t1;
	std::cout << "Data read time: " << duration.count() << " seconds" << std::endl;


	double rho = Pearson(testMatrix,refMatrix);
	std::cout << "Pearson correlation: " << rho << std::endl;
	
	
	double rho_y = sqrt(1 - rho * rho);
	testMatrix.scale(rho);
	refMatrix.scale(rho_y);

	refMatrix + testMatrix;
	Matrix* res = SpatialFilter(refMatrix);
	auto t3 = high_resolution_clock::now();
	duration = t3 - t2;
	std::cout << "Compute time: " << duration.count() << " seconds" << std::endl;

	return 0;




}



