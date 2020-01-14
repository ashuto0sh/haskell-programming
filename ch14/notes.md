# Testing

*Spec testing* is a somewhat newer version of unit testing. Like unit testing, it tests specific functions independently and asks you to assert that, when given the declared input, the result of the operation will be equal to the desired result. Haskell provides libraries for both unit and spec testing.

*Property tests* test the formal properties of programs without requiring formal proofs by allowing you to express a truth-valued, universally quantified (that is, will apply to all cases) function – usually equality – which will then be checked against randomly generated inputs.

Property testing is fantastic for ensuring that you’ve met the minimum requirements to satisfy laws, such as the laws of monads or basic associativity. It is not appropriate for all programs, though, as it is not useful for times when there are no assertable, truth-valued properties of the software.
