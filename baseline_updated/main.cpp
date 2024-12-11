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
    double* tmpMatrix = nullptr;
    double maxRho = 0.0;

    testMatrix=(double*)malloc(sizeof(double)*SIZE);
    refMatrix=(double*)malloc(sizeof(double)*SIZE);
    tmpMatrix=(double*)malloc(sizeof(double)*SIZE);
  

        ReadData(string(PATH) + "Itest6.dat",testMatrix);


 
    for (int i = 0; i < LAG; ++i) {
        string path = string(PATH) + "Iref6" + string(1, 'A' + i) + ".dat";
        ReadData(path,refMatrix);


        //-----------------------AR(1)-------------------------
        double rho = Pearson(testMatrix, refMatrix);
        
	Add(refMatrix, testMatrix,rho);

        //-----------update the result--------
        double r = Pearson(refMatrix, testMatrix);
       
        std::cout << "Pearson correlation: " << fixed << std::setprecision(10) << rho << std::endl;
       	if (r > maxRho) {
            maxRho = r;
            memcpy(tmpMatrix, refMatrix, sizeof(double) * SIZE);
        }

    }


        //-----------SpatialFilter and AnomalyDetection--------
	Sub(testMatrix,tmpMatrix);//a-b->b
    	SpatialFilter(tmpMatrix);
	writeData("./res.txt",tmpMatrix);
	AnomalyDetection(tmpMatrix);


    auto t3 = high_resolution_clock::now();
    duration<double> duration = t3 - t1;
    std::cout << "Compute time: " << duration.count() << " seconds" << std::endl;

    //-----------------------Write Data--------------------
    //writeData("./res.txt",finalRes);
    return 0;
}

