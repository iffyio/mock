extern crate mock_common;
extern crate mock_derive;

#[cfg(test)]
mod tests {
    use mock_common::Expectation;
    use mock_derive::mock_trait;

    #[test]
    fn mock_traits() {
        {
            #[mock_trait(MyTraitMock)]
            trait MyTrait {
                fn a(&self, arg1: i32) -> &'static str;
                fn rat(&self) -> &'static str;
                fn cat(&self, foo: String) -> Option<Option<String>>;
            }

            let mock = MyTraitMock::new();

            mock.expect_a().return_value("boo");
            mock.expect_rat().return_value("rat");

            assert_eq!("boo", mock.a(99));
            assert_eq!("boo", mock.a(99));
            assert_eq!("rat", mock.rat());
        }

        {
            #[mock_trait(MyTraitMock)]
            trait MyTrait {
                fn add_panda(&self, name: String, age: usize, is_happy: bool);
                fn is_happy_panda(&self, name: &'static str) -> bool;
            }

            let mock = MyTraitMock::new();

            mock.expect_add_panda().return_with(|(_, _, _)| Some(()));
            mock.expect_is_happy_panda().return_with(|panda| {
                Some(panda == "gao gao" || panda == "ming ming" || panda == "bei bei")
            });

            assert_eq!((), mock.add_panda("boo".to_owned(), 2, false));
            assert_eq!((), mock.add_panda("foo".to_owned(), 3, true));
            assert_eq!(true, mock.is_happy_panda("ming ming"));
            assert_eq!(false, mock.is_happy_panda("bao bao"));
            assert_eq!(true, mock.is_happy_panda("gao gao"));
        }
    }
}
