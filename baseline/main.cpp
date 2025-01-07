#include"matrix.h"
using namespace std;
using namespace std::chrono;
#include <iomanip>
#define LAG 18
#define PATH "/home/jiahuaz/ChangeDetection/test6/"
int main() {


    //-------------------Read Data------------------------

	double* testMatrix = nullptr;
	double* refMatrix = nullptr;
	double* finalRes = nullptr;
	double minRho = 1.0;

	testMatrix=(double*)malloc(sizeof(double)*SIZE);
	refMatrix=(double*)malloc(sizeof(double)*SIZE);
	finalRes=(double*)malloc(sizeof(double)*SIZE);


	ReadData(string(PATH) + "Itest6.dat",testMatrix);

	duration<double> duration(0);
 
	for (int i = 0; i < LAG; ++i) {
		string path = string(PATH) + "Iref6" + string(1, 'A' + i) + ".dat";
		ReadData(path,refMatrix);

		
		auto t1 = high_resolution_clock::now();
		//-----------------------AR(1)-------------------------
		double rho = Pearson(testMatrix, refMatrix);
		//std::cout << "Pearson correlation: " << fixed << std::setprecision(10) << rho << std::endl;
		
		Add(refMatrix, testMatrix,rho);
		//-----------SpatialFilter and AnomalyDetection--------
		SpatialFilter(refMatrix);
		
		
	       // writeData("./res.txt",refMatrix);
		AnomalyDetection(refMatrix);

		//-----------update the result--------
		double r = Pearson(refMatrix, testMatrix);
		if (r < minRho&&i!=2) {
			minRho = r;
			memcpy(finalRes, refMatrix, sizeof(double) * SIZE);
			printf("%lf,%d",minRho,i);
		}


		auto t2 = high_resolution_clock::now();
		duration+=t2-t1;
	}
	std::cout << "Compute time: " << duration.count() << " seconds" << std::endl;

	//-----------------------Write Data--------------------
	writeData("./18.txt",finalRes);
	return 0;
}

