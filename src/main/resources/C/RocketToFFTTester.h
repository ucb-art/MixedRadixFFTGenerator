// Test Setup
#define NUM_TESTS 2
#define TEST_MEM_SIZE 4101

// Test Memory Locations
#define toFFT_BASE 0
#define fromFFT_BASE 2048
#define fftIdx_BASE 4096
#define isFFT_BASE 4097
#define k_BASE 4098
#define setupDone_BASE 4099
#define calcDone_BASE 4100

// Useful Constants
// CHANGE START TO 0
#define DONE 1UL
#define START 1UL

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

//////////// TEST MEMORY -- REMOVE
unsigned long test[TEST_MEM_SIZE];
//////////// END TEST MEMORY

