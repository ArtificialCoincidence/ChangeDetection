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
#include<cuda_runtime.h>
using namespace std;
class Matrix {
private:
	const size_t row;
	const size_t col;
	vector<double> data;
	double* d_data;
public:
	Matrix(size_t r, size_t c) :row(r), col(c), data(r* c) {}
	double& operator()(size_t r, size_t c);//r and c start from 0.
	const double& operator()(size_t r, size_t c)const;
	Matrix& operator+(Matrix &m);
	Matrix& operator=(const Matrix &m);
	void readData(const string& filename);
	void writeData(const string& filename);
	friend double Pearson( Matrix& a, Matrix& b);
	friend void SpatialFilter(double* d_data,int rank);
	Matrix* spatialFilter(Matrix&a);
	friend void AnomalyDetection(Matrix& a);
	friend Matrix Scale(Matrix& a,double rho);

};


#endif

