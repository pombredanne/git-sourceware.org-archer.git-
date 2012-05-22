class A
{
};

template<typename T>
int foo (T, char)
{
  T t;
  return 11;
}

template<typename T, typename T2>
int foo2 (T, T2, char)
{
  T t;
  return 11;
}

namespace C
{
  namespace D {
    template<typename T, typename T2>
    int foo3 (T, T2, char)
    {
      T t;
      return 11;
    }
  }
}
int operator<< (A, int)
{
  return 12;
}

int operator< (A, int)
{
  return 13;
}

int operator<= (A, int)
{
  return 14;
}

template<typename T>
int operator==(T, char)
{
  return 15;
}

int operator==(A, int)
{
  return 16;
}

template<typename T>
class B{
  T t;
public:
  int operator==(int)
    {
      return 17;
    }
};

int operator== (B<int>, char){
  return 18;
}

template <class T, int>
class Outer{
 public:
  template <class T2, int>
    class Inner{
  public:
    template <int>
      class ReallyInner{};
  };
};


int main ()
{
  A a;
  
  foo (a, 'a');
  foo (a, 1);
  foo2 (a, a, 'a');
  C::D::foo3 (a, a, 'a');

  a << 22;
  a <  22;
  a <= 22;

  a == 'a';
  a == 1;

  B<int> b;
  b == 1;
  b == 'a';

  Outer<int, 23>::Inner<long, 27>::ReallyInner<5> oir;
  
  return 0;
}
