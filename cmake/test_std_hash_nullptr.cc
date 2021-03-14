/// Here, we will check if std::hash<nullptr_t> compiles.

#include <functional>

int main() {
  std::hash<std::nullptr_t> h;
  return h(nullptr);
}
