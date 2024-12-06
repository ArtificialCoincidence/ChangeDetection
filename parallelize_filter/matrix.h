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
#include<cuda_runtime.h>
#define ROW 500
#define COL 500
#define SIZE 250000
using namespace std;
void ReadData(const string& filename, double* data);
__global__ void Pearson(double* a, double* b,double* rho);
void Add(double* a, double*b,double rho);
__global__ void SpatialFilter(double* a);
void AnomalyDetection(double* a);
void writeData(const string& filename,double* data);





#endif

