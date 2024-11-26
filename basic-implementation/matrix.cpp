#include"matrix.h"
double& Matrix::operator()(size_t r, size_t c)  //r and c start from 0
{
	if (r >= row || r < 0 || c >= col || c < 0)
		throw out_of_range("Index out of bound");
	return data[row * r + c];
}

const double& Matrix::operator()(size_t r, size_t c)const
{
	if (r >= row || r < 0 || c >= col || c < 0)
		throw out_of_range("Index out of bound");
	return data[row * r + c];

}
Matrix& Matrix::operator+(Matrix& m) {
	int n = data.size();
	for (int i = 0; i < n; ++i)
		data[i] += m.data[i];
	return *this;
}

Matrix& Matrix::operator=(const Matrix& m)
{
	this->data = m.data;
	return *this;
}

void Matrix::readData(const string& filename) {
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
	in.close();

}

void Matrix::writeData(const string& filename) {
	ofstream out(filename);
	try {
		if (!out.is_open())
			throw runtime_error("Failed to open file: " + filename);

	}
	catch (const runtime_error& e) {
		std::cerr << "Error: " << e.what() << std::endl;
	}
	/*for (int i = 0; i < row; ++i)
	{
		for (int j = 0; j < col; ++j) {
			out << (*this)(i, j) << " ";
		}
		out << endl;
	}*/
	for (int i = 0; i < 250000; ++i)
		out << data[i] << ",";
	out << endl;
	out.close();

}


void Matrix::scale(double rho) {
	for (int i = 0; i < data.size(); ++i)
		data[i] *= rho;
}

Matrix Scale(Matrix& a, double rho) {
	Matrix tmp = a;
	for (int i = 0; i < tmp.data.size(); ++i)
		tmp.data[i] *= rho;
	return tmp;
}

double Pearson(Matrix& a, Matrix& b) {
	const vector<double>& x = a.data;
	const vector<double>& y = b.data;
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

Matrix* SpatialFilter(Matrix& a)
{
	Matrix* res = new Matrix(a.row, a.col);
	const int rank = 9;//9x9 kernel
	const int kernelSize = rank * rank;
	int offset[rank];
	int minOffset = -((rank - 1) >> 1);
	for (int i = 0; i < rank; ++i)
		offset[i] = minOffset++;
	/*
	-4 -3 -2 -1 0 1 2 3 4
	*/

	for (int i = 0; i < a.row; ++i) {
		for (int j = 0; j < a.col; ++j) {
			double sum = 0;
			for (auto di : offset) {
				for (auto dj : offset) {
					int r = max(0, i + di);
					int c = max(0, j + dj);
					r = min(static_cast<int>(a.row - 1), r);
					c = min(static_cast<int>(a.col - 1), c);
					sum += a(r, c);
				}
			}
			(*res)(i, j) = sum / kernelSize;

		}
	}

	return res;

}


void AnomalyDetection(Matrix& a) {
	vector <double> data = a.data;
	size_t size = a.row * a.col;
	sort(data.begin(), data.end());
	double percentileLow = 0.25;
	double percentileHigh = 0.9;
	double iqr = data[percentileHigh * size] - data[percentileLow * size];
	double thresholdDown = data[percentileLow * size]-1.5*iqr;
	double thresholdUp = data[percentileHigh * size]+1.5*iqr;
	for (double& v : a.data)
		if (v<thresholdDown||v>thresholdUp)//not sure
			v = 0;
	return;

}






