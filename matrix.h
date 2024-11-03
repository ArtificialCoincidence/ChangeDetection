#ifndef MATRIX_H_HEADER
#define MATRIX_H_HEADER
#include<string>
#include<iostream>
#include<fstream>
#include<stdio.h>
#include<vector>
#include<cmath>
#include<chrono>
#include <numeric>
#include<algorithm>
using namespace std;
class Matrix {
private:
	const size_t row;
	const size_t col;
	vector<double> data;
public:
	Matrix(size_t r, size_t c) :row(r), col(c), data(r* c) {}
	double& operator()(size_t r, size_t c);//r and c start from 0.
	void operator+(Matrix& m);
	void readData(string filename);
	friend double Pearson(Matrix& a, Matrix& b);
	friend Matrix* SpatialFilter(Matrix& a);
	friend void AnomalyDetection(Matrix& a);
	void scale(double rho);
};


#endif

