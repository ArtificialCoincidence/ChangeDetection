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
    double* refMatrices[LAG] ;
    double* as[LAG];
    double* finalRes = nullptr;
    double* rho=nullptr;
    double* maxRho ;
    
    cudaMallocManaged(&maxRho,sizeof(double));
    *maxRho=0.0;
    cudaMallocManaged(&finalRes,sizeof(double*));
    finalRes=nullptr;

    for(int i=0;i<LAG;++i){
    	cudaMallocManaged(&refMatrices[i], SIZE * sizeof(double));
        string path = string(PATH) + "Iref6" + string(1, 'A' + i) + ".dat";
        ReadData(path,refMatrices[i]);
    	cudaMallocManaged(&as[i], SIZE * sizeof(double));
	memcpy(as,refMatrix,sizeof(double)*SIZE);	
	}

    cudaMallocManaged(&testMatrix, SIZE * sizeof(double));
    ReadData(string(PATH) + "Itest6.dat",testMatrix);
    
    cudaMallocManaged(&rho,sizeof(double)*LAG);
    cudaMallocManaged(&finalRes, SIZE * sizeof(double));
	
   __global__ Lag(testMatrix,refMatrices,as,finalRes,rho,maxRho){
	int idx=blockIdx.x*blockDim.x+threadIdx.x;
	double* refMatrix=refMatrices[idx];
	double* a=as[idx];

	Pearson<<<1,512>>>(testMatrix, refMatrix,&rho[idx]);
	cudaDeviceSynchronize();
	printf("pearson:%f\n",rho[idx]);

        Add<<<128,512>>>(refMatrix, testMatrix,rho[idx]);
	cudaDeviceSynchronize();

        SpatialFilter<<<128,512>>>(refMatrix,a);
	cudaDeviceSynchronize();
 
 
        AnomalyDetection(refMatrix);
 
	Pearson<<<1,512>>>(refMatrix, testMatrix,&rho[idx]);
	cudaDeviceSynchronize();
   
	if(rho[idx]>*maxRho)
	{
		atomicExch(maxRho,rho[idx);
		atomicExch(finalRes,refMatrix);
	}
   
   }
 
	Lag<<<1,LAG>>>(testMatrix,refMatrices,as,finalRes,rho,maxRho);	

    auto t3 = high_resolution_clock::now();
    duration<double> duration = t3 - t1;
    std::cout << "Compute time: " << duration.count() << " seconds" << std::endl;

    //-----------------------Write Data--------------------
    return 0;
}

