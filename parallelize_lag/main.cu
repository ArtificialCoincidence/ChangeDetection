#include"matrix.h"
using namespace std;
using namespace std::chrono;
#include <iomanip>
#define LAG 15
#define PATH "/home/jiahuaz/ChangeDetection/test6/"
int main() {

    auto t1 = high_resolution_clock::now();

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
	
   
    cudaStream_t stream[LAG];
    for(int i=0;i<LAG;++i)
	    cudaStreamCreate(&stream[i]);
 
    for (int i = 0; i < LAG; ++i) {
        string path = string(PATH) + "Iref6" + string(1, 'A' + i) + ".dat";
        ReadData(path,refMatrix);


        //-----------------------AR(1)-------------------------
	Pearson<<<1,512>>>(testMatrix, refMatrix,rho);
	cudaDeviceSynchronize();
	std::cout << "Pearson correlation: " << fixed << std::setprecision(10) << *rho << std::endl;
        Add<<<128,512>>>(refMatrix, testMatrix,*rho);
	cudaDeviceSynchronize();
        //-----------SpatialFilter and AnomalyDetection--------
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


    }

    auto t3 = high_resolution_clock::now();
    duration<double> duration = t3 - t1;
    std::cout << "Compute time: " << duration.count() << " seconds" << std::endl;

    for(int i=0;i<LAG;++i)
	    cudaStreamDestroy(&stream[i]);

    //-----------------------Write Data--------------------
    return 0;
}

