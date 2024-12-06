#include"matrix.h"
using namespace std;
using namespace std::chrono;
#include <iomanip> 
#define LAG 15
#define PATH "/home/jiahuaz/ChangeDetection/test6/"

int main() {

	/*
	1. done------modify the filter from 3x3->9x9
	2. done------modify the anomaly detection with the ecdf.py
	3. ask oscar for the demo2 container and verify the result step by step
	4. use different lags select the lag whose corelation of result and testImage is the biggest(or smallest?) confirm from marcello again
	 	4.1 which reference images for which area
	5. parallelize the lags part 
	6. ask marcello how to plot the result
	7. profiling which part consume most of the time and further optimize
	8. write the report 
	*/

	/*
	possible errors
	1.anomaly detection,outliers???
	*/

	auto t1 = high_resolution_clock::now();

	//-------------------Read Data------------------------
	Matrix testMatrix(500, 500);
	Matrix refMatrix(500, 500);
	testMatrix.readData(string(PATH)+"Itest6.dat");

	double minRho = 1.0;
	Matrix finalRes(500,500);
	Matrix* res = new Matrix(500, 500);
	for (int i = 0; i < LAG; ++i) {
		string path = string(PATH)+"Iref6"+string(1,'A'+i) + ".dat";
		refMatrix.readData(path);


		//-----------------------AR(1)-------------------------
		double rho = Pearson(testMatrix, refMatrix);
		std::cout << "Pearson correlation: " <<fixed<<std::setprecision(10)<< rho << std::endl;
		double rho_y = sqrt(1 - rho * rho);
		Matrix tmp1 = Scale(refMatrix, rho_y);
		Matrix tmp2 = Scale(testMatrix, rho);
		*res = tmp1 + tmp2;
		
		//-----------SpatialFilter and AnomalyDetection--------

		res = SpatialFilter(*res);
		res->writeData(string(PATH) + "sf.txt");
		AnomalyDetection(*res);

		//-----------update the result--------
		double r = Pearson(*res, testMatrix);
		minRho = min(minRho, r);
		finalRes = r == minRho ? *res : finalRes;


	}

	auto t3 = high_resolution_clock::now();
	duration<double> duration = t3 - t1;
	std::cout << "Compute time: " << duration.count() << " seconds" << std::endl;

	//-----------------------Write Data--------------------
	//finalRes.writeData(string(PATH)+"res.txt");


	return 0;




}



