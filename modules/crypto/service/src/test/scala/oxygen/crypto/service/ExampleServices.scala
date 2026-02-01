package oxygen.crypto.service

import java.time.Duration
import oxygen.crypto.model.KeyFormat
import zio.durationInt

object ExampleServices {

  val tokenTimeToLive: Duration = 1.day
  val config: JWTService.Issuer.Config = JWTService.Issuer.Config(JWTService.Issuer.ValidAfter.Empty, JWTService.Issuer.Expiry.IssueDelay(tokenTimeToLive))

  private object keys {

    val hmac1: HashKey.Hmac = HashKey.Hmac(CryptoKey.HS256.fromPlain("example-hmac-key-1"))

    val hmac2: HashKey.Hmac = HashKey.Hmac(CryptoKey.HS256.fromPlain("example-hmac-key-2"))

    val rsa1: HashKey.RsaPair =
      HashKey.RsaPair(
        CryptoKey.RSA.Public.fromX509(
          KeyFormat.X509PEM(
            """-----BEGIN PUBLIC KEY-----
              |MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAyC+aKBY+6yt3c8uoMLgy
              |qTBLGvBO/jw3loYLmnMS7hjuYTVuBTnubpdV/ObgtggGDk3QYWryUakm9Z2+ZegP
              |k56wK1/OkPP8BVlqk+eYD5spjhd6YaDOeA91lSOfnhGj8xbpv7M21IABymAJGsTK
              |CH67c7kuYmHP7wSaIxGdIFyIjJBCqcRi9SDautTFCnAMHLuAR60xg2u4hnjkfxEd
              |7Kutvusz78eEmp/NDMDAnPHnaXcLvJBaYSrMC95MIh3vlfO4DRUBD8hR+TFNfY5u
              |v4mEdMLgF0FYJ96r2TD9IdT8d9t9SLxAc+X/Oog7X+0lmM4frdwGyoHSHx0lQ//F
              |bQIDAQAB
              |-----END PUBLIC KEY-----""".stripMargin,
          ),
        ),
        CryptoKey.RSA.Private.fromPKCS8(
          KeyFormat.PKCS8PEM(
            """-----BEGIN PRIVATE KEY-----
              |MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDIL5ooFj7rK3dz
              |y6gwuDKpMEsa8E7+PDeWhguacxLuGO5hNW4FOe5ul1X85uC2CAYOTdBhavJRqSb1
              |nb5l6A+TnrArX86Q8/wFWWqT55gPmymOF3phoM54D3WVI5+eEaPzFum/szbUgAHK
              |YAkaxMoIfrtzuS5iYc/vBJojEZ0gXIiMkEKpxGL1INq61MUKcAwcu4BHrTGDa7iG
              |eOR/ER3sq62+6zPvx4San80MwMCc8edpdwu8kFphKswL3kwiHe+V87gNFQEPyFH5
              |MU19jm6/iYR0wuAXQVgn3qvZMP0h1Px3231IvEBz5f86iDtf7SWYzh+t3AbKgdIf
              |HSVD/8VtAgMBAAECggEAQ6SaIYuKzSpN3cnmVjgXnkXcSwkCmDO+0NUQnKrfraas
              |/hnwyEFQCPyIKMHXEotZiIHQtOpPYTqYGIyp5oNoqAgKHU3tXdujwCb7tWifYD6c
              |5w+V4eFVgwouQCveZqotTRnj+EvVbi6m7DxRN0NhxUcVjj/3pXTJr0lN0CXGg0VI
              |NnwmY0vXg3IjVzT+DQOJ3pCjcIrDdmG1JOQ3MbhPd/OA9g9Jh2oP5zR5FxABitSr
              |Gv4I3/fxLjZBYlAoopMJXz0Ctv5WrRInMaOGj2m67+MfRumgM/5XNquKPwLfmcLD
              |VJp61IU9Ly7KVMM9iHrZ4jD9kv40MaYS63C1/KStQQKBgQDmqKlFgIpk9A0YxH+b
              |2thQuweAvkjTfWaPIbC4N0kKv1WjQGs6S/Jz6r0aiRHQI8Oj5oHfOdlzaLvJ2s8G
              |Ra0J3VIYHSSU8ISMRb0u4Nbeevn1tXkUNNdJUgnUqGhfN/rdhOPeBmt9tSa4Pu6b
              |qmrk1laIYj/dOk3QDvmV3/j5fQKBgQDeLeIZtaI7gzbMjCAhoxVkcq3cFF24Wl9t
              |0q4pXjPTOSOsoW6PCRexwHhUKmctqcnvCKNOZ61l5jud4KEX74TisHQxnlXU/cPs
              |3oewS06U4n7CP1RxJy3xUnD8G30KerJgP448DLXMzyPJeqyo+NG5lggD0or7Vfuo
              |PwITsVs+sQKBgQCwLFJdi3go816tdCgjWluuDawFYVEud7FF0W2wxkk7tDgsoJ9F
              |v4xHjRsykU7Rsv9SyFSZsw2rYGtOGZnWKJGp3yk80cWHHM2gdgOxMnnrI/KXASyh
              |ilqE3ew0SQyu7YrmdwG0KyMeuKYCQ87rhpZugmdFdAVja4zTRRzMpGz94QKBgHTB
              |l/f6w26S3dFQMfJ7SVqoCUWGTln64/tKo8uJfaKafGzTyM3R8Wmvw4w3XFFa0IuU
              |9nzVbwIFo36r9PwnGqYmJGmO03xCfxuTNasnZ/xFqmdZ8H4mILaYpFdX+rXvFJ4P
              |yCc2ZrNih2Px25pQZ6Yneb7IfdDmtoEL24ONyG2BAoGAb/PyFDRtKSOZCOPfFSuX
              |r7/j/WqAKaYP+M202YCl4vT0lu9h2qu8L83GO26Fr9QV9SyPDNz8hZGLYvBsy2Vy
              |hskqlo+9rmoIxrd3AYDC1rJOkOkDmNmt7WBKPsV2rGdrEsQEP0A6gsVN8ruVKSph
              |3TQR5BCyHtKITEPg9z4Vyk4=
              |-----END PRIVATE KEY-----""".stripMargin,
          ),
        ),
      )

    val rsa2: HashKey.RsaPair =
      HashKey.RsaPair(
        CryptoKey.RSA.Public.fromX509(
          KeyFormat.X509PEM(
            """
              |----- BEGIN PUBLIC KEY-----
              |MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvsJ8hgLdktNpJgLrtTOu
              |hVggHd6ePiduLtCaII/Z27MvpWVjWcqcv2w9zGCd1VQK8UQD17iCeLm3Z/y1ehAs
              |FQ0AyE7i7M1SjraXgnbCt6CIsKY0fW/wFbztNa6LrTF7zkFFwHOGlKYYtkBTgWrA
              |75oI5UJySzM+cCGk+wNk4RddKafidS13OX7ddbE8K+kcdzQAQG4NgvAILxlldNH+
              |bfymq8oGMaJnilslVfRWmJdrdlRpaPD77vqtVIH8zxPe2hXkrU5dJ0rbagf1R7tM
              |uRSh0vOHGZXnsU8aswL3SfXJ7wnlqP49zBxUzPZwGf7uq22dBgRz6GA130jzogzh
              |xQIDAQAB
              |-----END PUBLIC KEY-----
              |""".stripMargin,
          ),
        ),
        CryptoKey.RSA.Private.fromPKCS8(
          KeyFormat.PKCS8PEM(
            """
              |-----BEGIN PRIVATE KEY-----
              |MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQC+wnyGAt2S02km
              |Auu1M66FWCAd3p4+J24u0Jogj9nbsy+lZWNZypy/bD3MYJ3VVArxRAPXuIJ4ubdn
              |/LV6ECwVDQDITuLszVKOtpeCdsK3oIiwpjR9b/AVvO01routMXvOQUXAc4aUphi2
              |QFOBasDvmgjlQnJLMz5wIaT7A2ThF10pp+J1LXc5ft11sTwr6Rx3NABAbg2C8Agv
              |GWV00f5t/KarygYxomeKWyVV9FaYl2t2VGlo8Pvu+q1UgfzPE97aFeStTl0nSttq
              |B/VHu0y5FKHS84cZleexTxqzAvdJ9cnvCeWo/j3MHFTM9nAZ/u6rbZ0GBHPoYDXf
              |SPOiDOHFAgMBAAECggEAd/9L6dkfnN8Ug+7Oa1wpaoQ3H9MMZDjVnOloKxeSihu7
              |2yVeS/uiguAKN+iHIM0PXRnV+Pt+R4c8ElPrrO9Da7KUTW0iXqgCSPgfcthWQMDh
              |e43x8v5ZXrNL3LsB7l70bJiu0hNaE2Qwo2TvoreOyqpw8hbMYvXHTy465BQMf3Ll
              |f8gYs8P/wB8LaJbB9QOYcjPGQYvGy1xA8czP/NyJrMHhZdKCxlxdq57TfOk/Qwf2
              |rTj67c/ijUjWWRLMRkNSENdMawa/JSW6oIL4iZIkrQizDH2XmuY0iH8lMuOTSex6
              |We/QA75U/BAG+A5kDlN+ZCRWnFQvl7TafP6iMpxewQKBgQD1amHn+dZ/OTt1Izyr
              |HKCn38qcdmV1Agemws+tMvynyMbAuDGlwH97HmvhEpGJCqLUxDjAshsq9hWppVJ+
              |HNQEWEa/fZC06fmzGdeH4knYd4GnY23Y4FkcNQj8askk4Ij6PQjuNcuQI1vlB2m/
              |8i0SqsclDYaS2Ox8AAn5a2UanQKBgQDG/Kb7oz0dW5C+5k5/ea6us+zVVMHqQ/Wh
              |TsskoWJxmYbVjCSDT1+hDHbKx1/L4Ezt5aNW5y2+NjZzSP31S2Hq8wP0+PoJ+v4G
              |FYKf3XjROFaIZNAPNzOuviK8ykBqXZpwM0zyztNkgbx7GPyZICjqICdTvUJHeJEP
              |MaQzORYHSQKBgQCIWQl6M5Fbb7W7987OFoKfojNUSGdH4c/Uc6mkRY6fvWnHSs6y
              |k0eAZL909bcL12/uG/KYBJwZe00/yo0RafzVSSJNY3rROKk1O2dGpI94h7TuBczS
              |0JpRlJmge8ezCFiyBtT+W770HuEAVuUG8oreMymfSWt6DLEel3rzKxqp9QKBgQCS
              |8g8qMyE1peHdZ9w+PRDnyM0zuTS+mFMzPSm9MkStmW++EZEBO0NmffrXwqYWu9xz
              |riqSSYQ5PHW3awbH91QnGjEo3hhf+G9Q9N+SyQ2oi48ziCcxpVVQTO8ANeed5OPg
              |ybGgeWYYicozqX8YYWx1gSspptDkUL/m1YTpotWGqQKBgCjQz93RghPBmOYDAT7W
              |zMrV+Xd2WkjNOk3wSZINeBgNC6AxasD63cV8kyNwZT04p6kJrmpO1jrXlRRpmgaw
              |3hQ66qyXbT3BsG2fuxDaKbsSGUiWeBVoAL26SIESjmJ0x1ZYpJn3M5Dm35PztYTV
              |VGjYsc+Mg7GMQk8vGsMGGqqB
              |-----END PRIVATE KEY-----
              |""".stripMargin,
          ),
        ),
      )

  }

  object bearerTokenServices {
    val none: BearerTokenService.Issuer = BearerTokenService.Issuer.Live(SignatureService.Signer.Live(HashKey.Noop), SignatureService.Validator.Live(HashKey.Noop))
    val hmac1: BearerTokenService.Issuer = BearerTokenService.Issuer.Live(SignatureService.Signer.Live(keys.hmac1), SignatureService.Validator.Live(keys.hmac1))
    val hmac2: BearerTokenService.Issuer = BearerTokenService.Issuer.Live(SignatureService.Signer.Live(keys.hmac2), SignatureService.Validator.Live(keys.hmac2))
    val rsa1: BearerTokenService.Issuer = BearerTokenService.Issuer.Live(SignatureService.Signer.Live(keys.rsa1), SignatureService.Validator.Live(keys.rsa1))
    val rsa2: BearerTokenService.Issuer = BearerTokenService.Issuer.Live(SignatureService.Signer.Live(keys.rsa2), SignatureService.Validator.Live(keys.rsa2))
  }

  object jwtServices {
    val none: JWTService.Issuer.Std[Person] = JWTService.Issuer.StdLive(bearerTokenServices.none, config)
    val hmac1: JWTService.Issuer.Std[Person] = JWTService.Issuer.StdLive(bearerTokenServices.hmac1, config)
    val hmac2: JWTService.Issuer.Std[Person] = JWTService.Issuer.StdLive(bearerTokenServices.hmac2, config)
    val rsa1: JWTService.Issuer.Std[Person] = JWTService.Issuer.StdLive(bearerTokenServices.rsa1, config)
    val rsa2: JWTService.Issuer.Std[Person] = JWTService.Issuer.StdLive(bearerTokenServices.rsa2, config)
  }

}
