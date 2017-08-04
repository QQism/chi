(ns rst.core-test
  (:require [clojure.test :refer :all]
            [rst.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(deftest another-test
  (testing "This is a test"
    (is (= "Hello QQ" (greeting "QQ")))))

(deftest test-again
  (testing "test"
    (is (= "<div>Test</div>" (test-html "Test")))))
