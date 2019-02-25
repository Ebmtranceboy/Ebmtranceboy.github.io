#include <math.h>
#include <stdlib.h>
#define DIM 102
#define SIZE (DIM * DIM)
#define MIN(a,b) (a<b?a:b)
#define MAX(a,b) (a>b?a:b)

double diff, visc;
double t = 0.0f, dt;
static double density[SIZE], s[SIZE];
static double Vx[SIZE], Vy[SIZE];
static double Vx0[SIZE], Vy0[SIZE];
int iter;

void diffuse (int, double*, double* , double ,double );
void set_bnd(int , double* );
void lin_solve(int , double*, double* , double , double );
void project(double* , double* , double* , double* );
void advect(int , double* ,double* ,double* ,double* ,double );
void addDensity(int , int , double);
void addVelocity(int, int, double, double);

double* density_ref() {
  return density;
}

int constrainInt(int x, int a, int b){
  return MIN(MAX(x,a),b);
}

double constrainDouble(double x, double a, double b){
  return MIN(MAX(x,a),b);
}

int IX(int x, int y) {
  x = constrainInt(x, 0, DIM-1);
  y = constrainInt(y, 0, DIM-1);
  return x + (y * DIM);
}

void addDensity(int x, int y, double amount){
  density[IX(x,y)] += amount;
}

void addVelocity(int x, int y, double amountX, double amountY){
  int index = IX(x,y);
  Vx[index] += amountX;
  Vy[index] += amountY;
}

double vx;
double vy;
double alpha;

void fluid_init(){
  dt = 0.2f;
  diff = 0.000005f;
  visc = 0.000000001f;
  iter = 16;
  int i,j;
  for (i = -5; i <= 1; i++) {
    for (j = -1; j <= 1; j++) {
      addDensity(DIM/2+i, DIM/2+j, 50*(double)random()/RAND_MAX * 110.0f + 50.0f);
    }
  }
  
  for (i = 0; i < 2; i++) {
    alpha += (double)random() * acos(-1.0f) / RAND_MAX / 30.0f;
    double sens = (double)random() / RAND_MAX;
    if(sens<0.03) alpha=-alpha;
    vx = 1.8f * cos(alpha);
    vy = 1.8f * sin(alpha);
    t += 0.01f;
    addVelocity(DIM/2.0f, DIM/2.0f, vx, vy);
  }
}

/* * * * * * FLUID ALGORITHM * * * * * */

void fluid_step(){
 
    diffuse(1, Vx0, Vx, visc, dt);
    diffuse(2, Vy0, Vy, visc, dt);

    project(Vx0, Vy0, Vx, Vy);

    advect(1, Vx, Vx0, Vx0, Vy0, dt);
    advect(2, Vy, Vy0, Vx0, Vy0, dt);

    project(Vx, Vy, Vx0, Vy0);

    diffuse(0, s, density, diff, dt);
    advect(0, density, s, Vx, Vy, dt);
}

void  diffuse (int b, double* x, double* x0, double diff,double dt) {
    double a = dt * diff * (DIM -2) * (DIM - 2);
    lin_solve(b, x, x0, a, 1.0f + 6.0f * a);
  }
  
void  set_bnd(int b, double* x) {
    int i,j;
    for (i = 1; i < DIM - 1; i++) {
      x[IX(i, 0  )] = b == 2 ? -x[IX(i, 1  )] : x[IX(i, 1 )];
      x[IX(i, DIM-1)] = b == 2 ? -x[IX(i, DIM-2)] : x[IX(i, DIM-2)];
    }
    for (j = 1; j < DIM - 1; j++) {
      x[IX(0, j)] = b == 1 ? -x[IX(1, j)] : x[IX(1, j)];
      x[IX(DIM-1, j)] = b == 1 ? -x[IX(DIM-2, j)] : x[IX(DIM-2, j)];
    }

    x[IX(0, 0)] = 0.5f * (x[IX(1, 0)] + x[IX(0, 1)]);
    x[IX(0, DIM-1)] = 0.5f * (x[IX(1, DIM-1)] + x[IX(0, DIM-2)]);
    x[IX(DIM-1, 0)] = 0.5f * (x[IX(DIM-2, 0)] + x[IX(DIM-1, 1)]);
    x[IX(DIM-1, DIM-1)] = 0.5f * (x[IX(DIM-2, DIM-1)] + x[IX(DIM-1, DIM-2)]);
  }
  
void  lin_solve(int b, double*x,double* x0, double a, double c) {
    double cRecip = 1.0f / c;
    int i,j,k;
    for (k = 0; k < iter; k++) {
      for (j = 1; j < DIM - 1; j++) {
        for (i = 1; i < DIM - 1; i++) {
          x[IX(i, j)] =
             (x0[IX(i, j)]
               + a*(    x[IX(i+1, j)]
                 +x[IX(i-1, j)]
                 +x[IX(i, j+1)]
                 +x[IX(i, j-1)]
          )) * cRecip;
        }
      }
      set_bnd(b, x);
    }
  }

void project(double*velocX, double* velocY,double* p, double* div) {
  int i,j;
  for (j = 1; j < DIM - 1; j++) {
    for (i = 1; i < DIM - 1; i++) {
      div[IX(i, j)] = -0.5f*(
        velocX[IX(i+1, j)]
        -velocX[IX(i-1, j)]
        +velocY[IX(i, j+1)]
        -velocY[IX(i, j-1)]
        )/DIM;
      p[IX(i, j)] = 0.0f;
    }
  }

  set_bnd(0, div); 
  set_bnd(0, p);
  lin_solve(0, p, div, 1.0f, 6.0f);

  for (j = 1; j < DIM - 1; j++) {
    for (i = 1; i < DIM - 1; i++) {
      velocX[IX(i, j)] -= 0.5f * (  p[IX(i+1, j)]
        -p[IX(i-1, j)]) * DIM;
      velocY[IX(i, j)] -= 0.5f * (  p[IX(i, j+1)]
        -p[IX(i, j-1)]) * DIM;
    }
  }
  set_bnd(1, velocX);
  set_bnd(2, velocY);
}

void advect(int b, double* d,double* d0,double* velocX,double* velocY,double dt) {
  int i0, i1, j0, j1;

  double dtx = dt * (DIM - 2.0f);
  double dty = dt * (DIM - 2.0f);

  double s0, s1, t0, t1;
  double tmp1, tmp2, x, y;

  double Nfloat = DIM;
  float ifloat, jfloat;
  int i, j;

  for (j = 1, jfloat = 1.0f; j < DIM - 1; j++, jfloat++) { 
    for (i = 1, ifloat = 1.0f; i < DIM - 1; i++, ifloat++) {
      tmp1 = dtx * velocX[IX(i, j)];
      tmp2 = dty * velocY[IX(i, j)];
      x    = ifloat - tmp1; 
      y    = jfloat - tmp2;

      if (x < 0.5f) x = 0.5f; 
      if (x > Nfloat + 0.5f) x = Nfloat + 0.5f; 
      i0 = (int)floor(x); 
      i1 = i0 + 1;
      if (y < 0.5f) y = 0.5f; 
      if (y > Nfloat + 0.5f) y = Nfloat + 0.5f; 
      j0 = (int)floor(y);
      j1 = j0 + 1; 

      s1 = x - (double)i0; 
      s0 = 1.0f - s1; 
      t1 = y - (double)j0; 
      t0 = 1.0f - t1;

      // DOUBLE CHECK THIS!!!
      d[IX(i, j)] = 
        s0 * (t0 * d0[IX(i0, j0)] + t1 * d0[IX(i0, j1)]) +
        s1 * (t0 * d0[IX(i1, j0)] + t1 * d0[IX(i1, j1)]);
    }
  }
  set_bnd(b, d);
}

/* * * * * * * END ALGORITHM * * * * * */

double RGB[3];

double* hsv2rgb(double h, double s, double v) {
  double p, q, t, ff;
  int i;

  if (s <= 0.0f) { // < is bogus, just shuts up warnings
    RGB[0] = v;
    RGB[1] = v;
    RGB[2] = v;
    return RGB;
  }
  if (h >= 360.0f)	h = 0.0f;
	h /= 60.0f;
	i = (int)floor(h);
	ff = h - (double)i;
	p = v * (1.0f - s);
	q = v * (1.0f - (s * ff));
	t = v * (1.0f - (s * (1.0f - ff)));
	
	switch(i){
	  case 0:
	    RGB[0] = v;
	    RGB[1] = t;
	    RGB[2] = p;
	    break;
	    
	  case 1:
	    RGB[0] = q;
	    RGB[1] = v;
	    RGB[2] = p;
	    break;
	    
	  case 2:
	    RGB[0] = p;
	    RGB[1] = v;
	    RGB[2] = t;
	    break;
	    
	  case 3:
	    RGB[0] = p;
	    RGB[1] = q;
	    RGB[2] = v;
	    break;
	    
	  case 4:
	    RGB[0] = t;
	    RGB[1] = p;
	    RGB[2] = v;
	    break;
	    
	  case 5:
	    RGB[0] = v;
	    RGB[1] = p;
	    RGB[2] = q;
	    break;
	    
	 }
	 
	 return RGB;
}
