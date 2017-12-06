#include <stdio.h>
#include <stdlib.h>

#define NB 2
#define NTPB 4 //Minimum number of threads in this program has to be 64
#define sizeTab
void testCUDA(cudaError_t error, const char *file, int line)  {
   if (error != cudaSuccess) {
      printf("There is an error in file %s at line %d : %d\n", file, line, error);
       exit(EXIT_FAILURE);
   } 
}

#define testCUDA(error) (testCUDA(error, __FILE__ , __LINE__))

__device__ int Bd[NB*NTPB+1];
__device__ int Ad[NB*NTPB+1];


__device__ void printRange(int idx,int* t, int d,int f){
   
   if(idx == 1){
      for (int i = d; i < f; ++i)
      {
         printf("\ttab[%d] = %d\n", i,t[i]);
      }
      printf("\n");   
   }

      // printf("tab[%d:%d]\", d,f);
   
}


__device__ void merge(int idx,int* C,int* A, int ab,int ad,int* B, int bb, int bd, int pos,int size){
   int start = ab+bb;
   int i = 0;
   while(1){
      if(ab == ad && bb == bd){

         printRange(idx,C,0,16);
         return;
      }else if(ab == ad){
         C[start+i] = B[bb];
         bb++;
      }else if(bb == bd){
         C[start+i] = A[ab];
         ab++;
      }else if( A[ab] > B[bb]){
         C[start+i] = B[bb];
         bb += 1;
      }else{
         C[start+i] = A[ab];
         ab++;
      }
      i++;
   }
}



__global__ void mergePath(int* C,int* A,int lenA,int *B,int lenB){
   int idx = threadIdx.x + blockIdx.x * blockDim.x+1;
   Ad[idx] = 0;
   Bd[idx] = 0;
   if(idx == 0){
      Ad[NB*NTPB] = lenA;
      Bd[NB*NTPB] = lenB;
   }
   
   int index = (idx*(lenA+lenB))/(NB*NTPB);
   int atop = (index > lenA)? lenA-1: index;
   int btop = (index > lenA)?  index - (lenA-1) : 0;
   int abot = btop;
   //int i = 0;
   int offset;
   int ai;
   int bi;
   int flag = 0;
   while(!flag){
      //printf("%d\n",i++ );
      offset = (int)floor(((float)(atop - abot))/(float)(2));
      ai = atop - offset;
      bi = btop + offset;
      // if(idx == 7){
      //    printf("%d : (%d,%d) off: %d, atop %d btop %d abot %d\n",idx,ai,bi,offset,atop,btop,abot );
      // }
      if( bi == 0 || ai >= lenA || A[ai] > B[bi-1]){
         if(A[ai-1] <= B[bi])
         {
            printf("%d : (%d,%d)\n",idx,ai,bi );
            Ad[idx] = ai;
            Bd[idx] = bi;
            flag = 1;
         }
         else{
            atop = ai - 1;
            btop = bi + 1;
         }
      }
      else{
         abot = ai + 1;
      }
   }
   __syncthreads();
   if(idx == 1)
   {
      printRange(1,Ad,0,9);
      printRange(1,Bd,0,9);
   }
   int size = (lenA+lenB)/(NB*NTPB);
   for (int i = 1; i <= NB*NTPB; ++i)
   {
      merge(idx,C,A,Ad[i-1],Ad[i],B,Bd[i-1],Bd[i],i-1,size);
     // printRange(0,C,0,16);
   }
}




void affiche(int* A,int len){
   for (int i = 0; i < len; ++i)
   {
      printf("%d, ",A[i] );
   }
   puts("\b\b  ");
}
#define s 8
int main(int argc,char** argv){
   /*int A[s] = {5,17,11,19,18,15,16,114,119,120,14,112,117,159
                  ,71,66,58,157,42,195,185,15,176,1124,1419
                  ,120,147,112,1147,19,71,646
               };*/
   // int C[2*s] = {0};
   int* C = (int*)calloc(4*s,sizeof(int));
   int A[s] = {1,2,3,4,6,10,11,13};
   int B[s] = {5,7,8,9,12,14,15,16};
   int *aGPU,*bGPU,*cGPU;
   cudaMalloc(&aGPU,s*sizeof(int));
   cudaMemcpy(aGPU,A,s*sizeof(int),cudaMemcpyHostToDevice);
   cudaMalloc(&bGPU,s*sizeof(int));
   cudaMemcpy(bGPU,B,s*sizeof(int),cudaMemcpyHostToDevice);
   cudaMalloc(&cGPU,4*s*sizeof(int));
   cudaMemset(cGPU,0,2*s*sizeof(int));
 
   mergePath<<<NB,NTPB>>>(cGPU,aGPU,s,bGPU,s);

   testCUDA(cudaMemcpy(C,cGPU,2*s*sizeof(int),cudaMemcpyDeviceToHost));
   affiche(C,2*s);
}


// __global__ void merge(int* A,int lenTab){
//    int idx = threadIdx.x + blockIdx.x * blockDim.x;
//    int bidx = threadIdx.x;
//    int turn = 1;
//    int end = 0;
//    int size = lenTab/(NB*NTPB);
//    if(size < 2){
//       size = 2;
//    }
//    __shared__ int *sT;
//    //premier tri de size element

//    //tri A[idx*size : (idx+1)*size-1]
//    for (int i = 0; i < size; i++)
//    {
//       sT[bidx+i] = A[idx+i];

//    }
//    for (int j = 0; j < size; j++)
//    {
//       for (int k = j; k < size; k++)
//       {
//          if( sT[j] > sT[k]){
//             int tmp = sT[j];
//             sT[j] = sT[k];
//             sT[k] = sT[j];
//          }
//       }
//    }
//    turn++;
//    size *=2;
//    for (int i = 0; i < size; i++)
//    {
//       // sT[bidx+i] = A[idx+i];
//       A[idx] = sT[bidx];
//    }
//    // while(!end){

//    // }


//    //suite 


// }


// void mergepath(int* C,int *A,int lenA,int* B, int lenB,int nbT){
//    int Adiag[nbT+1] = {0}
//    int Bdiag[nbT+1] = {0}

//    Adiag[nbT] = lenA
//    Bdiag[nbT] = lenB
//    for( int i = 0; )
// }