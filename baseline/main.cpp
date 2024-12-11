#include"matrix.h"
using namespace std;
using namespace std::chrono;
#include <iomanip>
#define LAG 15 
#define PATH "/home/jiahuaz/ChangeDetection/test6/"
int main() {

    auto t1 = high_resolution_clock::now();

    //-------------------Read Data------------------------

    double* testMatrix = nullptr;
    double* refMatrix = nullptr;
    double* finalRes = nullptr;
    double minRho = 1.0;

    testMatrix=(double*)malloc(sizeof(double)*SIZE);
    refMatrix=(double*)malloc(sizeof(double)*SIZE);
    finalRes=(double*)malloc(sizeof(double)*SIZE);
  

/*    double *debug=(double*)malloc(sizeof(double)*9);

    for(int i=0;i<9;++i)
	    debug[i]=i+1;
    SpatialFilter(debug);
    for(int i=0;i<9;++i)
	    printf("%f,",debug[i]);
	writeData("./res.txt",debug);
  */
        ReadData(string(PATH) + "Itest6.dat",testMatrix);


 
    for (int i = 0; i < LAG; ++i) {
        string path = string(PATH) + "Iref6" + string(1, 'A' + i) + ".dat";
        ReadData(path,refMatrix);


        //-----------------------AR(1)-------------------------
        double rho = Pearson(testMatrix, refMatrix);
        std::cout << "Pearson correlation: " << fixed << std::setprecision(10) << rho << std::endl;
        
	Add(refMatrix, testMatrix,rho);
        //-----------SpatialFilter and AnomalyDetection--------
        SpatialFilter(refMatrix);
	
	
        writeData("./res.txt",refMatrix);
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

