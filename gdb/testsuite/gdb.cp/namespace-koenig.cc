namespace A
{
  class C
  {
  public:
    static const int x = 11;
  };

  int
  first(C c)
  {
    return 11;
  }

  int
  first(int a, C c)
  {
    return 22;
  }

  int
  second(int a, int b, C cc, int c, int d)
  {
    return 33;
  }

}

struct B
{
  A::C c;
};

int
main()
{
  A::C c;
  B b;

  A::first(c);
  first(0, c);
  second(0, 0, c, 0, 0);
  A::first(b.c);

  return first(0, c);
}
