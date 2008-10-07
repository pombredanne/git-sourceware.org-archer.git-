namespace A
{
  int a = 1;
  int x = 2;
}

namespace C
{
  int cc = 3;
}

using namespace C;
int marker5()
{
  cc;
  return 0;
}

int marker4()
{
  using A::x;
  return marker5();
}

int marker3()
{
  return marker4();
}

int marker2()
{
  namespace B = A;
  B::a;
  return marker3();
}

int marker1()
{
  int total = 0;
    {
      int b = 1;
        {
          using namespace A;
          int c = 2;
            {
              int d = 3;
              total = a + b + c + d + marker2(); // marker1 stop
            }
        }
    }
  return total;
}

int main()
{
  using namespace A;
  a;
  return marker1();
}
