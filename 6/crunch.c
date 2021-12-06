#include <stdio.h>

typedef long long Int;

Int loop(int i,Int r0,Int r1,Int r2,Int r3,Int r4,Int r5,Int r6,Int r7,Int r8){
  Int spawns;

  while(i > 0){
    spawns = r0;
    r0 = r1;
    r1 = r2;
    r2 = r3;
    r3 = r4;
    r4 = r5;
    r5 = r6;
    r6 = r7;
    r7 = r8;
    r8 = 0;

    r6 += spawns;
    r8 += spawns;

    i--;
  }

  return r0+r1+r2+r3+r4+r5+r6+r7+r8;
}

int main(){
  int days;
  Int r0,r1,r2,r3,r4,r5,r6,r7,r8;
  fscanf(stdin,"%d",&days);
  fscanf(stdin,"%lld",&r0);
  fscanf(stdin,"%lld",&r1);
  fscanf(stdin,"%lld",&r2);
  fscanf(stdin,"%lld",&r3);
  fscanf(stdin,"%lld",&r4);
  fscanf(stdin,"%lld",&r5);
  fscanf(stdin,"%lld",&r6);
  fscanf(stdin,"%lld",&r7);
  fscanf(stdin,"%lld",&r8);
  Int answer = loop(days,r0,r1,r2,r3,r4,r5,r6,r7,r8);
  printf("%lld\n", answer);
  return 0;
}

