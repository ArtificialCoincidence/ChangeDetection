#include"matrix.h"
using namespace std;
using namespace std::chrono;
#include <iomanip>
#define LAG 1 
#define PATH "/home/jiahuaz/ChangeDetection/test6/"
int main() {

    auto t1 = high_resolution_clock::now();

    //-------------------Read Data------------------------

    double* testMatrix = nullptr;
    double* refMatrix = nullptr;
    double* finalRes = nullptr;
    double minRho = 1.0;

//    cudaMallocManaged(&testMatrix, SIZE * sizeof(double));
//    cudaMallocManaged(&refMatrix, SIZE * sizeof(double));
//    cudaMallocManaged(&finalRes, SIZE * sizeof(double));
    testMatrix=(double*)malloc(sizeof(double)*SIZE);
    refMatrix=(double*)malloc(sizeof(double)*SIZE);
    finalRes=(double*)malloc(sizeof(double)*SIZE);
    ReadData(string(PATH) + "Itest6.dat",testMatrix);


 
    for (int i = 0; i < LAG; ++i) {
        string path = string(PATH) + "Iref6" + string(1, 'A' + i) + ".dat";
        ReadData(path,refMatrix);


        //-----------------------AR(1)-------------------------
        double rho = Pearson(testMatrix, refMatrix);
        std::cout << "Pearson correlation: " << fixed << std::setprecision(10) << rho << std::endl;
        
	Add(refMatrix, testMatrix,rho);
        //-----------SpatialFilter and AnomalyDetection--------
    writeData("./res.txt",refMatrix);
        SpatialFilter(refMatrix);
	
	AnomalyDetection(refMatrix);

        //-----------update the result--------
        double r = Pearson(refMatrix, testMatrix);
        if (r < minRho) {
            minRho = r;
            memcpy(finalRes, refMatrix, sizeof(double) * SIZE);
        }


    }

    auto t3 = high_resolution_clock::now();
    duration<double> duration = t3 - t1;
    std::cout << "Compute time: " << duration.count() << " seconds" << std::endl;

    //-----------------------Write Data--------------------
    //writeData("./res.txt",finalRes);
    return 0;
}

