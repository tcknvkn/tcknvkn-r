# tcknvkn (R)

`tcknvkn`, R projelerinde Türkiye Cumhuriyeti Kimlik Numarası (TCKN) ve Vergi Kimlik Numarası (VKN) doğrulaması yapmak için geliştirilmiş hafif bir kütüphanedir.

## Kurulum

Kaynak koddan çalıştırmak için:

```r
source("R/tcknvkn.R")
```

Paket arşivi üretmek için:

```bash
R CMD build .
```

## Hızlı Başlangıç

```r
source("R/tcknvkn.R")

validate_tckn("10000000146")$valid
validate_vkn("1000036109")$valid
```

## API Özeti

- `validate_tckn(input)`
- `validate_multiple_tckn(inputs)`
- `validate_vkn(input)`
- `validate_multiple_vkn(inputs)`

Her çağrı aşağıdaki formatta sonuç döndürür:

```r
list(
  valid = TRUE/FALSE,
  value = "normalize_edilmis_deger",
  errors = c("hata1", "hata2")
)
```

## Sık Kullanım İfadeleri

- tc üret
- vkn üret
- tc uret
- vergi no üret
- vergi no oluşturucu
- tckn üret
- vkn algoritması
- tc no uret
- vkn doğrulama algoritması
- tc no üret
- tc oluştur

## İlgili Bağlantılar

- [Kütüphaneler](https://www.tcknvkn.com/kutuphaneler)
- [R kütüphane sayfası](https://www.tcknvkn.com/kutuphaneler/r)
- [TC üret](https://www.tcknvkn.com/tc-uret)
- [TC no üret](https://www.tcknvkn.com/tc-no-uret)
- [TC üretici](https://www.tcknvkn.com/tc-uretici)
- [TCKN üret](https://tcknvkn.com/tckn-uret)
- [Vergi no üret](https://www.tcknvkn.com/vergi-no-uret)
- [Vergi no üretici](https://www.tcknvkn.com/vergi-no-uretici)
- [VKN üret](https://tcknvkn.com/vkn-uret)

## Test

```bash
Rscript tests/test_tcknvkn.R
```

## Lisans

MIT