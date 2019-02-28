#define A 159
#define len (4 * A * A)
#define FORWARD 1
#define BACKWARD -1
#define INT 0
#define EXT 1
#define UP 1
#define DOWN -1

static unsigned char pre[len], run[len], post[len];
static int cmm[3], cmp[3], cpm[3], cpp[0];
static unsigned char smm[3], smp[3], spm[3], spp[3];
static unsigned char rmm[3], rmp[3], rpm[3], rpp[3];

int move = FORWARD;

void backgroundBlack( unsigned char* t){
  for(int i=0; i<A*A; i++){
    t[4*i+0] = (unsigned char)0;
    t[4*i+1] = (unsigned char)0;
    t[4*i+2] = (unsigned char)0;
    t[4*i+3] = (unsigned char)255;
  }
}

void setColor( unsigned char* arr, int x, int y,  unsigned char* color){
  for(int i=0; i<4; i++)
    arr[4*(x*A+y)+i] = color[i];
}

void ini(int x, int y, int pos, int age){
   unsigned char coul1[] = {255,0,0,255};
   unsigned char coul2[] = {0,0,255,255};
   unsigned char coul3[] = {0,255,0,255};
   unsigned char coul4[] = {255,0,0,255};
   
  if(age == FORWARD)
    if(pos == INT) setColor(post, x, y, coul3);
    else {
      setColor(post, x-1, y-1, coul2);
      setColor(post, x+1, y-1, coul2);
      setColor(post, x+1, y+1, coul2);
      setColor(post, x-1, y+1, coul2);
    }
  else 
    if(pos == INT) setColor(run, x, y, coul1);
    else {
      setColor(run, x-1, y-1, coul4);
      setColor(run, x+1, y-1, coul4);
      setColor(run, x+1, y+1, coul4);
      setColor(run, x-1, y+1, coul4);
    }
}

void carre(int r, int pos, int age){
  ini(r, r, pos, age);
  ini(r, A-1-r, pos, age);
  ini(A-1-r, A-1-r, pos, age);
  ini(A-1-r, r, pos, age);
}

void losg(int r, int pos, int age){
  ini((A-1)/2, (A-1)/2-r, pos, age);
  ini((A-1)/2, (A-1)/2+r, pos, age);
  ini((A-1)/2-r, (A-1)/2, pos, age);
  ini((A-1)/2+r, (A-1)/2, pos, age);
}

void init(){
  backgroundBlack(pre);
  backgroundBlack(run);
  backgroundBlack(post);
  
  carre(3, EXT, FORWARD);
  carre(8, EXT, BACKWARD);
  losg(4, EXT, BACKWARD);

  carre(12, INT, FORWARD);
  carre(8, EXT, FORWARD);
  losg(8, INT, FORWARD);
}

void swap( unsigned char* u,  unsigned char*v){
  unsigned char temp;
  for(int i=0; i<len; i++){
    temp = u[i];
    u[i] = v[i];
    v[i] = temp;
  }
} 

void ahead(){
 for(int i=0; i<len ; i++){
   pre[i] = run[i];
   run[i] = post[i];
 }
}

void init3( int*t){
  t[0]=0;
  t[1]=0;
  t[2]=0;
}

unsigned char xbt(int bit, unsigned char  ent){
  return (unsigned char)((ent & (1<<bit)) != 0 ? 1 : 0);
}

void map( unsigned char*t,  unsigned char* ref, int i, int v){
  t[0] = xbt(i, ref[v+0]);
  t[1] = xbt(i, ref[v+1]);
  t[2] = xbt(i, ref[v+2]);
}

unsigned char* embroidery_step(int choice){
   
  switch(choice){
    case DOWN:
      if(move == FORWARD){
        move = BACKWARD;
        swap(post,pre);
      }
      break;
    case UP:
      if(move == BACKWARD){
        move = FORWARD;
        swap(post,pre);
      }
      break;
  }
  
  ahead();
  backgroundBlack(post);
  
  int mm, mp, pm, pp;
	int x, y, r, i, c;
	int is[]={0,7,6};
	
	for(y=0; y<A; y++) {
    for(x=0; x<A; x++) {
		  r = 4*(y*A + x);

	    mm = r;
	    mp = r+4;
	    pm = r+4*A;
	    pp = r+4*A+4;

	    if(y == A-1) {
		    pm = 4*x;
		    pp = 4*x+4;
		  }
	    if(x == A-1) {
		    mp = 4*y*A;
		    if (y == A-1) pp=0; else pp = 4*(y+1)*A;
		  }
	    init3(cmm);
	    init3(cmp);
	    init3(cpm);
	    init3(cpp);

	    for(i=0; i<3; i++) {
		    map(smm, run, is[i], mm);
		    map(smp, run, is[i], mp);
		    map(spm, run, is[i], pm);
		    map(spp, run, is[i], pp);
		    map(rmm, post, is[i], mm);
		    map(rmp, post, is[i], mp);
		    map(rpm, post, is[i], pm);
		    map(rpp, post, is[i], pp);
		    
		    for(c=0; c<3; c++){
				  if((!((smm[c]&&smp[c])||(smm[c]&&spm[c])||(smm[c]&&spp[c])||(smp[c]&&spm[c])||(smp[c]&&spp[c])||(spm[c]&&spp[c])))&&(smm[c]||smp[c]||spm[c]||spp[c])) {
					  rmm[c] = spp[c];
				    rmp[c] = spm[c];
				    rpm[c] = smp[c];
				    rpp[c] = smm[c];
				  }

					rmm[c] ^= (unsigned char)xbt(is[i],pre[mm+c]);
					    
					cmm[c] += rmm[c]<<is[i];
					cmp[c] += rmp[c]<<is[i];
					cpm[c] += rpm[c]<<is[i];
					cpp[c] += rpp[c]<<is[i];
			  }
			}
			for(c=0; c<3; c++){
        post[mm+c] = (unsigned char)cmm[c];
        post[mp+c] =(unsigned char) cmp[c];
        post[pm+c] = (unsigned char)cpm[c];
        post[pp+c] = (unsigned char)cpp[c];
		  } 
		}
	}
	return run
	;
}
