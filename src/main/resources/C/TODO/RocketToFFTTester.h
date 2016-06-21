// Test Setup (num_tests ~ # of fft sizes in 1 config to test)
#define NUM_TESTS 2
#define TEST_MEM_SIZE 4101

// Base of FFT-used interface memory
// #define TEST_BASE 0x48000000UL
#define TEST_BASE 0

// Test Memory Locations
#define toFFT_OFFSET 0
#define fromFFT_OFFSET 2048
#define fftIdx_OFFSET 4096
#define isFFT_OFFSET 4097
#define k_OFFSET 4098
#define setupDone_OFFSET 4099
#define calcDone_OFFSET 4100

// Useful Constants
// #define START 0UL
#define START 1UL
#define DONE 1UL

// All Frames of Inputs/Outputs for FFTN, (iN,oN)
unsigned long i3[6] = {1,2,3,4,5,6};
unsigned long o3[6] = {13,14,15,16,17,18};
unsigned long i4[8] = {4,5,6,7,8,9,10,11};
unsigned long o4[8] = {16,17,18,19,20,21,22,23};

// FFTN tested
int fftn[NUM_TESTS] = {3,4};

// # of Frames
int frames[NUM_TESTS] = {2,2};

// FFT Index (for Setup)
unsigned long fftIdx[NUM_TESTS] = {0,1};

// FFT? 1 ; IFFT? 0
unsigned long isFFT[NUM_TESTS] = {1,1};

// Pointers to input/output arrays
unsigned long *inptr[NUM_TESTS]={i3,i4};
unsigned long *outptr[NUM_TESTS]={o3,o4};

// Used Functions
int setup(unsigned long currFFTIdx, unsigned long currIsFFT);
int load(int testNum, int idxStart, int currN);
void calculate (void);
// REMOVE
int loadOutput(int testNum, int idxStart, int currN);
int checkOut(int testNum, int idxStart, int currN);

//////////// TEST MEMORY -- REMOVE
unsigned long test[TEST_MEM_SIZE];
//////////// END TEST MEMORY

