#include <stdio.h>

typedef long long Int;

Int r0;
Int r1;
Int r2;
Int r3;
Int r4;
Int r5;
Int r6;
Int r7;
Int r8;

void loop99(){
  for(int i=0; i<11; i++){
    r7 += r0;
    r8 += r1;
    r0 += r2;
    r1 += r3;
    r2 += r4;
    r3 += r5;
    r4 += r6;
    r5 += r7;
    r6 += r8;
  }
}

// i must be > 0
void days(int i){

  for(;;){

    r7 += r0;
    if(--i == 0) break;

    r8 += r1;
    if(--i == 0) break;

    r0 += r2;
    if(--i == 0) break;

    r1 += r3;
    if(--i == 0) break;

    r2 += r4;
    if(--i == 0) break;

    r3 += r5;
    if(--i == 0) break;

    r4 += r6;
    if(--i == 0) break;

    r5 += r7;
    if(--i == 0) break;

    r6 += r8;
    if(--i == 0) break;

  }

}



int main(){
  Int nDays;

  fscanf(stdin,"%lld",&nDays);
  fscanf(stdin,"%lld",&r0);
  fscanf(stdin,"%lld",&r1);
  fscanf(stdin,"%lld",&r2);
  fscanf(stdin,"%lld",&r3);
  fscanf(stdin,"%lld",&r4);
  fscanf(stdin,"%lld",&r5);
  fscanf(stdin,"%lld",&r6);
  fscanf(stdin,"%lld",&r7);
  fscanf(stdin,"%lld",&r8);

  Int chunks    = nDays / 99;
  Int remainder = nDays % 99;

  //printf("ndays = %lld\n", nDays);
  //printf("chunks = %lld\n", chunks);
  //printf("remainder = %lld\n", remainder);

  for(Int i=0; i<chunks; i++){
    loop99();
  }

  if(remainder > 0) days(remainder);

  Int answer = r0+r1+r2+r3+r4+r5+r6+r7+r8;

  printf("%lld\n", answer);

  return 0;
}

