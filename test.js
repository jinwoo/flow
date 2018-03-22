function hello(fn, b) {
  return fn() + b;
}

function say() {
  return 'hello';
}

hello(say, 42);

// console.log(say());
