#include"matrix.h"
__device__ double gMeanX=0.0;
__device__ double gMeanY=0.0;
__device__ double gSumXY=0.0;
__device__ double gSumX2=0.0;
__device__ double gSumY2=0.0;
void ReadData(const string& filename, double* data) {
	ifstream in(filename);
	try {
		if (!in.is_open())
			throw runtime_error("Failed to open file: " + filename);
	}
	catch (const runtime_error& e) {
		std::cerr << "Error: " << e.what() << std::endl;
	}

	for (int i = 0; i < SIZE; ++i) {
		in >> data[i];
	}
	in.close();

}


__global__ void Pearson(double *x, double* y,double* rho) {
	int idx=blockDim.x*blockIdx.x+threadIdx.x;
	double lSumX=0.0,lSumY=0.0;
	for(int i=idx;i<SIZE;i+=blockDim.x*gridDim.x)
	{
		lSumX+=x[i];
		lSumY+=y[i];
	}
	atomicAdd(&gMeanX,lSumX);
	atomicAdd(&gMeanY,lSumY);
	__syncthreads();
	if(idx==0){
		gMeanX/=SIZE;
		gMeanY/=SIZE;
		//printf("gMeanX:%lf,gMeanY:%lf\n",gMeanX,gMeanY);
	}
	__syncthreads();
	
	double lSumXY = 0.0, lSumX2 = 0.0, lSumY2 = 0.0;
	for(int i=idx;i<SIZE;i+=blockDim.x*gridDim.x)
	{
		double dx = x[i] - gMeanX;
		double dy = y[i] - gMeanY;
		lSumXY += dx * dy;
		lSumX2 += dx * dx;
		lSumY2 += dy * dy;
		//if(idx==0) printf("intermidiatelSumxy:%lf,sumx2:%lf,sumy2:%lf\b",lSumXY,lSumX2,lSumY2);
	}
	atomicAdd(&gSumX2,lSumX2);
	atomicAdd(&gSumY2,lSumY2);
	atomicAdd(&gSumXY,lSumXY);
	__syncthreads();
	if(idx==0){	
		double denominator = sqrt(gSumX2) * sqrt(gSumY2);
		*rho= gSumXY / denominator;
		//printf("gSumXY:%lf,gSUMX2:%lf,gSumY2:%lf\n",gSumXY,gSumX2,gSumY2);
		gMeanX=gMeanY=gSumXY=gSumX2=gSumY2=0;
	}
}


__global__ void SpatialFilter(double* ref,double*a)
{
	const int rank = 9;//9x9 kernel
	const int kernelSize = rank * rank;
	int offset[rank];
	int minOffset = -((rank - 1) >> 1);
	

	for (int i = 0; i < rank; ++i)
		offset[i] = minOffset++;
	/*
	-4 -3 -2 -1 0 1 2 3 4
	*/
	int idx=blockIdx.x*blockDim.x+threadIdx.x;
	for (int tmp = idx; tmp < SIZE; tmp+=gridDim.x*blockDim.x) {
		int i=tmp/COL;
		int j=tmp%COL;
		double sum = 0;
		for (auto di : offset) {
			for (auto dj : offset) {
				int r = max(0, i + di);
				int c = max(0, j + dj);
				r = min(static_cast<int>(ROW - 1), r);
				c = min(static_cast<int>(COL - 1), c);
				sum += a[r*COL+c];
	//			if(tmp==idx&&idx==1)
		//			printf("%lf,",a[r*COL+c]);
			}
		}
		ref[tmp] = sum / kernelSize;
		//if(idx==0) printf("idx:%d,sum:%lf\n",tmp,sum);
	}
	return ;

}




__global__ void Add(double* ref, double* test, double rho) {
	int idx=blockIdx.x*blockDim.x+threadIdx.x;
	double rho_y = sqrt(1 - rho * rho);
	for (int i =idx; i < SIZE;i+=gridDim.x*blockDim.x)
	{
	       double refi=ref[i]*rho_y;
               double testi=test[i]*rho;
               ref[i] = testi>refi?testi+refi:0;
	}
	return;

}



void AnomalyDetection(double* a) {
        double data[SIZE];
        memcpy(data, a, sizeof(double) * SIZE);
        sort(data,data+SIZE);

//	thrust::device_ptr<double> data(a);
//	thrust::sort(data, data+SIZE);
	double percentileLow = 0.25;
	double percentileHigh = 0.9;
	double iqr = data[static_cast<int>(percentileHigh * SIZE)] - data[static_cast<int>(percentileLow * SIZE)];
	double thresholdDown = data[static_cast<int>(percentileLow * SIZE)] - 1.5 * iqr;
	double thresholdUp = data[static_cast<int>(percentileHigh * SIZE)] + 1.5 * iqr;
	for (int i=0;i<SIZE;++i)
		if (a[i]>=thresholdDown && a[i]<=thresholdUp)//not sure
			a[i] = 0;
}


void writeData(const string& filename,double *data ) {
	ofstream out(filename);
	try {
		if (!out.is_open())
			throw runtime_error("Failed to open file: " + filename);

	}
	catch (const runtime_error& e) {
		std::cerr << "Error: " << e.what() << std::endl;
	}
	for (int i = 0; i < SIZE; ++i)
		out << data[i] << ",";
	out << endl;
	out.close();

}








