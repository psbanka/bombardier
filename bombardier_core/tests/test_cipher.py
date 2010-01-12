"""Unit test for Cipher.py"""

import bombardier_core.Cipher
import unittest

class TestCipher(unittest.TestCase):
    def test_round_trip(self):                          
        """Round-trip test data"""
        test_data = "normal stuff"
        password = "abc123"
        cipher = bombardier_core.Cipher.Cipher(password)
        cipher_text = cipher.encrypt_string(test_data)
        round_trip = cipher.decrypt_string(cipher_text)
        self.assertEqual(test_data, round_trip)
    
if __name__ == "__main__":
    unittest.main()   
