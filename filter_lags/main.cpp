#include"matrix.h"
using namespace std;
using namespace std::chrono;

int main() {

	auto t1 = high_resolution_clock::now();

	//-------------------Read Data------------------------
	Matrix testMatrix(500,500);
	Matrix refMatrix(500, 500);
	Matrix refMatrix2(500, 500);
	Matrix refMatrix3(500, 500);
	vector<Matrix*> ref = { &refMatrix,&refMatrix2,&refMatrix3 };

	//Modify the file path if the file is moved.
	testMatrix.readData("/home/jiahuaz/ChangeDetection/Demos/SampleData/Itest0.dat");
	refMatrix.readData("/home/jiahuaz/ChangeDetection/Demos/SampleData/Iref0A.dat");
	refMatrix2.readData("/home/jiahuaz/ChangeDetection/Demos/SampleData/Iref0B.dat");
	refMatrix3.readData("/home/jiahuaz/ChangeDetection/Demos/SampleData/Iref0C.dat");

	auto t2 = high_resolution_clock::now();
	duration<double> duration = t2 - t1;
	std::cout << "Data read time: " << duration.count() << " seconds" << std::endl;

	vector<Matrix*> ref2;
	for (Matrix* reference : ref) {
		//--------------------------AR(1)----------------------
		double rho = Pearson(testMatrix, *reference);
		std::cout << "Pearson correlation: " << rho << std::endl;
		double rho_y = sqrt(1 - rho * rho);

		Matrix* res = new Matrix(500, 500);
		Matrix tmp1 = Scale(*reference, rho);
		Matrix tmp2 = Scale(testMatrix, rho_y);
		*res = tmp1 + tmp2;

		//-----------SpatialFilter and AnomalyDetection--------

		res = SpatialFilter(*res);
		AnomalyDetection(*res);
		ref2.push_back(res);

	}

	vector<double>c;
	for (auto p : ref2){
		c.push_back(Pearson(*p, testMatrix));
		delete p;
	}
	auto t3 = high_resolution_clock::now();
	duration = t3 - t2;
	std::cout << "Compute time: " << duration.count() << " seconds" << std::endl;


	//-----------------------Write Data--------------------
	//res->writeData("C:/Users/chang/Desktop/Demos/SampleData/test0/res.dat");


	return 0;




}



