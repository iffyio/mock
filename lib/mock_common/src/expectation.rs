pub trait Expectation<InputArgTypes, ReturnType> {
    fn return_value(self, value: ReturnType);
    fn return_with<P>(self, predicate: P) where P : Fn(InputArgTypes) -> Option<ReturnType> + 'static;
}
