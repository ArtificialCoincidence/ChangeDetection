#ifndef MATRIX_H_HEADER
#define MATRIX_H_HEADER
#include<string>
#include<iostream>
#include<cstring>
#include<fstream>
#include<stdio.h>
#include<vector>
#include<cmath>
#include<chrono>
#include <numeric>
#include<algorithm>
//#include<cuda_runtime.h>
#define ROW 500
#define COL 500
#define SIZE 250000
using namespace std;
void ReadData(const string& filename, double* data);
double Pearson(double* a, double* b);
void Add(double* a, double*b,double rho);
void Sub(double* a,double*b);
void SpatialFilter(double* a);
void AnomalyDetection(double* a);
void writeData(const string& filename,double* data);





#endif

