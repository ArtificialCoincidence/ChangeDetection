#include"matrix.h"
double& Matrix::operator()(size_t r, size_t c)//r and c start from 0
{
	if (r >= row || r < 0 || c >= col || c < 0)
		throw out_of_range("Index out of bound");
	return data[row * r + c];
}


void Matrix::operator+(Matrix& m) {
	int n = data.size();
	for (int i = 0; i < n; ++i)
		data[i] += m.data[i];
	return;
}


void Matrix::readData(string filename) {
	ifstream in(filename);
	try {
		if (!in.is_open())
			throw runtime_error("Failed to open file: " + filename);
	}
	catch (const runtime_error& e) {
		std::cerr << "Error: " << e.what() << std::endl;
	}

	for (int i = 0; i < row * col; ++i) {
		in >> data[i];
	}

}

void Matrix::scale(double rho) {
	for (int i = 0; i < data.size(); ++i)
		data[i] *= rho;
}

double Pearson(Matrix& a, Matrix& b) {
	vector<double>& x = a.data;
	vector<double>& y = b.data;
	if (x.size() != y.size())
		throw std::invalid_argument("Vectors x and y must have the same size.");
	int n = x.size();

	double meanX = accumulate(x.begin(), x.end(), 0.0) / n;
	double meanY = accumulate(y.begin(), y.end(), 0.0) / n;

	double sumXY = 0.0, sumX2 = 0.0, sumY2 = 0.0;
	for (int i = 0; i < n; ++i) {
		double dx = x[i] - meanX;
		double dy = y[i] - meanY;
		sumXY += dx * dy;
		sumX2 += dx * dx;
		sumY2 += dy * dy;
	}
	double denominator = sqrt(sumX2) * sqrt(sumY2);
	if (denominator == 0) {
		throw runtime_error("Division by zero in correlation calculation");
	}
	return sumXY / denominator;
	
}

Matrix* SpatialFilter(Matrix& a)
{
	Matrix* res = new Matrix(a.row,a.col);
	const int rank = 3;//3x3 kernel
	const int kernelSize = rank * rank;
	int offset[rank];
	int minOffset = -((rank - 1) >> 1);
	for (int i = 0; i < rank; ++i)
		offset[i] = minOffset++;//{-1,0,1}

	for (int i = 0; i < a.row; ++i) {
		for (int j = 0; j < a.col; ++j) {
			double sum = 0;
			for (auto di : offset) {
				for (auto dj : offset) {
					int r = max(0, i + di);
					int c= max(0, j + dj);
					r = min(static_cast<int>(a.row - 1), r);
					c = min(static_cast<int>(a.col - 1), c);
					sum +=a(r, c);
				}
			}
			(*res)(i, j) = sum / kernelSize;

		}
	}

	return res;
	
}

void AnomalyDetection(Matrix& a) {
	vector <double> data = a.data;
	sort(data.begin(), data.end());
	double thresholdUp,thresholdDown;
	//need tobe modify
	//
	//
	//
	//
	for (double& v : a.data)
		if (v > thresholdDown && v < thresholdDown)
			v = 0;
	return;

}