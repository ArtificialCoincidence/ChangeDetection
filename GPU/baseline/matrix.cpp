#include"matrix.h"
#include <iomanip>
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


double Pearson(double *x, double* y) {
	//double meanX = accumulate(x, x+SIZE, 0.0)/static_cast<double>SIZE;
	double meanX = accumulate(x, x+SIZE, 0.0)/SIZE;
	double meanY = accumulate(y, y+SIZE, 0.0)/SIZE;
	double sumXY = 0.0, sumX2 = 0.0, sumY2 = 0.0;
	for (int i = 0; i < SIZE; ++i) {
		double dx = x[i] - meanX;
		double dy = y[i] - meanY;
		sumXY += dx * dy;
		sumX2 += dx * dx;
		sumY2 += dy * dy;
	}
	double denominator = sqrt(sumX2) * sqrt(sumY2);
	try {
		if (denominator == 0) {
			throw runtime_error("Division by zero in correlation calculation");
		}
	}
	catch (const std::runtime_error& e) {
		cerr << "Error: " << e.what() << std::endl;
	}
	return sumXY / denominator;

}


void SpatialFilter(double* ref)
{
	const int rank = 9;//9x9 kernel
	const int kernelSize = rank * rank;
	int offset[rank];	
	double a[SIZE];
	memcpy(a,ref,SIZE*sizeof(double));	
	int minOffset = -((rank - 1) >> 1);
	for (int i = 0; i < rank; ++i)
		offset[i] = minOffset++;
	/*
	-4 -3 -2 -1 0 1 2 3 4
	*/

	for (int i = 0; i < ROW; ++i) {
		for (int j = 0; j < COL; ++j) {
			double sum = 0;
			for (auto di : offset) {
				for (auto dj : offset) {
					int r = max(0, i + di);
					int c = max(0, j + dj);
					r = min(static_cast<int>(ROW - 1), r);
					c = min(static_cast<int>(COL - 1), c);
					sum += a[r*COL+c];
			//		if(i==0&&j==1) 
			//			printf("%lf,",a[r*COL+c]);
				}
			}
			ref[i*COL+j] = sum / kernelSize;

		}
	}
	return ;

}




void Add(double* ref, double* test, double rho) {
	double rho_y = sqrt(1 - rho * rho);
	for (int i = 0; i < SIZE; ++i){
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
	double percentileLow = 0.25;
	double percentileHigh = 0.9;
	double iqr = data[static_cast<int>(percentileHigh * SIZE)] - data[static_cast<int>(percentileLow * SIZE)];
	double thresholdDown = data[static_cast<int>(percentileLow * SIZE)] - 1.5 * iqr;
	double thresholdUp = data[static_cast<int>(percentileHigh * SIZE)] + 1.5 * iqr;
	for (int i=0;i<SIZE;++i)
		if (a[i]>=thresholdDown && a[i]<=thresholdUp)//not sure
			a[i] = 0;
	return;

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
	{

		if(i==SIZE-1) {
			out<<data[i];
		}	
		else if(data[i]==0.0){
			out << std::fixed << std::setprecision(1);
			out << data[i] << ",";
		}
		else{

			out << std::fixed << std::setprecision(10);
			out<<data[i]<<",";
		}

	}
	out << endl;
	out.close();

}








