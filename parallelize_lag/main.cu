#include"matrix.h"
using namespace std;
using namespace std::chrono;
#include <iomanip>
#define LAG 1
#define PATH "/home/jiahuaz/ChangeDetection/test6/"
__global__ void LagPearson(double* testMatrix,double**refMatrices,double* rho){
	int idx=blockIdx.x*blockDim.x+threadIdx.x;
	double* refMatrix=refMatrices[idx];
	Pearson<<<1,512>>>(testMatrix, refMatrix,&rho[idx]);
	cudaError_t err = cudaGetLastError();
   }
 
__global__ void LagAdd(double* testMatrix,double**refMatrices,double* rho){
	int idx=blockIdx.x*blockDim.x+threadIdx.x;
	double* refMatrix=refMatrices[idx];
	Add<<<128,512>>>(refMatrix,testMatrix,rho[idx]);
   }
__global__ void LagSF(double**refMatrices,double** as){
	int idx=blockIdx.x*blockDim.x+threadIdx.x;
	double* refMatrix=refMatrices[idx];
	double* a=as[idx];
	SpatialFilter<<<128,512>>>(refMatrix,a);
   }
int main() {


	//-------------------Read Data------------------------
	//testMatrix
	double* testMatrix = nullptr;
	cudaMallocManaged(&testMatrix, SIZE * sizeof(double));
	ReadData(string(PATH) + "Itest6.dat",testMatrix);
	
	//refMatrices and their copies
	double** refMatrices = nullptr;
	double** as=nullptr;
	cudaMallocManaged(&refMatrices,LAG*sizeof(double*));
	cudaMallocManaged(&as,LAG*sizeof(double*));
	for(int i=0;i<LAG;++i){
		cudaMallocManaged(&refMatrices[i], SIZE * sizeof(double));
		string path = string(PATH) + "Iref6" + string(1, 'A' + i) + ".dat";
		ReadData(path,refMatrices[i]);
		cudaMallocManaged(&as[i], SIZE * sizeof(double));
	}
	
	//rho	
	double* rho=nullptr;
	cudaMallocManaged(&rho,sizeof(double)*LAG);
	
	auto t1 = high_resolution_clock::now();
        //--------------------Launch Kernel--------------------
	LagPearson<<<1,LAG>>>(testMatrix,refMatrices,rho);	
	cudaDeviceSynchronize();
	
	LagAdd<<<1,LAG>>>(testMatrix,refMatrices,rho);	
	cudaDeviceSynchronize();
	for(int i=0;i<LAG;++i)
		memcpy(as[i],refMatrices[i],sizeof(double)*SIZE);	
	
	LagSF<<<1,LAG>>>(refMatrices,as);	
	cudaDeviceSynchronize();
	for(int i=0;i<LAG;++i)
		AnomalyDetection(refMatrices[i]);
	LagPearson<<<1,LAG>>>(testMatrix,refMatrices,rho);	
	cudaDeviceSynchronize();
	
	
	auto t3 = high_resolution_clock::now();
        //-----------------------Write Data--------------------
	double maxRho=0;
	int  maxIdx=-1;
	for(int i=0;i<LAG;++i)
	{
		//printf("rho:%lf\n",rho[i]);
		if(rho[i]>maxRho){
			maxRho=rho[i];
			maxIdx=i;
		}
	}
	writeData("res.txt",refMatrices[maxIdx]);
		

	
	//---------------------Memory Free---------------------
	

	//---------------------Compute Time--------------------
	duration<double> duration = t3 - t1;
	std::cout << "Compute time: " << duration.count() << " seconds" << std::endl;
	return 0;
}

