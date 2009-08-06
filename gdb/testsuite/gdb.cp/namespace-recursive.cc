namespace A{
  int ax = 9;
}

namespace B{
  using namespace A;
}

namespace C{
  using namespace B;
}

int main(){
  using namespace C;
  return ax;
}