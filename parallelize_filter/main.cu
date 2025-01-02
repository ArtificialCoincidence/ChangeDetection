#include"matrix.h"
using namespace std;
using namespace std::chrono;
#include <iomanip>
#define LAG 18
#define PATH "/home/jiahuaz/ChangeDetection/test6/"
int main() {

    	auto t0 = high_resolution_clock::now();

    //-------------------Read Data------------------------

	double* testMatrix = nullptr;
	double* refMatrix = nullptr;
	double* finalRes = nullptr;
	double* rho=nullptr;
	double minRho = 1.0;
	double* a=nullptr;
	cudaMallocManaged(&rho,sizeof(double));
	cudaMallocManaged(&testMatrix, SIZE * sizeof(double));
	cudaMallocManaged(&refMatrix, SIZE * sizeof(double));
	cudaMallocManaged(&finalRes, SIZE * sizeof(double));
	cudaMallocManaged(&a,SIZE*sizeof(double));
	ReadData(string(PATH) + "Itest6.dat",testMatrix);


	duration<double> duration(0);
	for (int i = 0; i < LAG; ++i) {
		string path = string(PATH) + "Iref6" + string(1, 'A' + i) + ".dat";
		ReadData(path,refMatrix);
		//-----------------------AR(1)-------------------------
		auto t1 = high_resolution_clock::now();
		Pearson<<<1,512>>>(testMatrix, refMatrix,rho);
		cudaDeviceSynchronize();
		Add<<<500,512>>>(refMatrix, testMatrix,*rho);
		cudaDeviceSynchronize();
		//-----------SpatialFilter and AnomalyDetection--------
		//std::cout << "Pearson correlation: " << fixed << std::setprecision(10) << *rho << std::endl;
		memcpy(a,refMatrix,sizeof(double)*SIZE);	
		SpatialFilter<<<128,512>>>(refMatrix,a);
		cudaDeviceSynchronize();
		AnomalyDetection(refMatrix);

		//-----------update the result--------
		Pearson<<<1,512>>>(refMatrix, testMatrix,rho);
		cudaDeviceSynchronize();
		if (*rho < minRho) {
		    minRho = *rho;
		    memcpy(finalRes, refMatrix, sizeof(double) * SIZE);
		}

		auto t2 = high_resolution_clock::now();
		duration+=t2-t1;
	}

    	auto t4 = high_resolution_clock::now();
	//duration=t4-t0;
	std::cout << "Compute time: " << duration.count() << " seconds" << std::endl;
	writeData("res.txt",finalRes);
	//-----------------------Write Data--------------------
	return 0;
}

