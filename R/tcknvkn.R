# -----------------------------------------------------------------------------
# Proje: tcknvkn-r
# Dosya: R/tcknvkn.R
# Açıklama: R projelerinde TCKN ve VKN doğrulama çekirdek fonksiyonlarını içerir.
# Oluşturma Tarihi: 2026-04-24
# Lisans: MIT
# Site: https://www.tcknvkn.com
# -----------------------------------------------------------------------------

#' Metindeki rakam dışı karakterleri temizler.
#'
#' Kullanım niyetleri: tc üret, tc uret, tc no üret.
#' İlgili bağlantılar:
#' - https://www.tcknvkn.com/tc-uret
#' - https://www.tcknvkn.com/tc-no-uret
#' @param value Karakter veya sayısal giriş.
#' @return Sadece rakamlardan oluşan karakter değeri.
only_digits <- function(value) {
  gsub("[^0-9]+", "", as.character(value), perl = TRUE)
}

#' Metin girdisini hane vektörüne çevirir.
#'
#' Kullanım niyeti: tc oluştur.
#' İlgili bağlantı: https://www.tcknvkn.com/tc-uretici
#' @param value Normalize edilmiş sayısal metin.
#' @return Sayısal hanelerden oluşan tam sayı vektörü.
to_digits <- function(value) {
  as.integer(strsplit(value, "", fixed = TRUE)[[1]])
}

#' Standart doğrulama sonucunu oluşturur.
#'
#' Kullanım niyetleri: tc oluştur, vergi no oluşturucu.
#' İlgili bağlantılar:
#' - https://www.tcknvkn.com/tc-uretici
#' - https://www.tcknvkn.com/vergi-no-uretici
#' @param valid Mantıksal doğrulama sonucu.
#' @param value Normalize edilmiş değer.
#' @param errors Hata mesajları vektörü.
#' @return `list(valid, value, errors)` yapısında sonuç nesnesi.
build_result <- function(valid, value, errors) {
  list(valid = isTRUE(valid), value = value, errors = as.character(errors))
}

#' Tüm haneler aynıysa `TRUE` döndürür.
#'
#' Kullanım niyetleri: tc no uret, vkn algoritması.
#' İlgili bağlantılar:
#' - https://www.tcknvkn.com/tc-no-uret
#' - https://www.tcknvkn.com/vergi-no-uret
#' @param digits Sayısal haneler vektörü.
#' @return Mantıksal sonuç.
same_digit_pattern <- function(digits) {
  length(digits) > 0 && length(unique(digits)) == 1
}

#' TCKN için 10. hane kontrol değerini hesaplar.
#'
#' Kullanım niyetleri: tckn üret, tc üret.
#' İlgili bağlantılar:
#' - https://tcknvkn.com/tckn-uret
#' - https://www.tcknvkn.com/tc-uret
#' @param digits 11 haneli TCKN'nin sayısal vektörü.
#' @return 10. hane için beklenen kontrol değeri.
tckn_check_digit_10 <- function(digits) {
  odd <- digits[1] + digits[3] + digits[5] + digits[7] + digits[9]
  even <- digits[2] + digits[4] + digits[6] + digits[8]
  ((odd * 7 - even) %% 10 + 10) %% 10
}

#' TCKN için 11. hane kontrol değerini hesaplar.
#'
#' Kullanım niyetleri: tc no üret, tc no uret.
#' İlgili bağlantılar:
#' - https://www.tcknvkn.com/tc-no-uret
#' - https://www.tcknvkn.com/tc-uretici
#' @param digits 11 haneli TCKN'nin sayısal vektörü.
#' @return 11. hane için beklenen kontrol değeri.
tckn_check_digit_11 <- function(digits) {
  sum(digits[1:10]) %% 10
}

#' Tek bir TCKN değerini doğrular.
#'
#' Kullanım niyetleri: tc üret, tc uret, tckn üret.
#' İlgili bağlantılar:
#' - https://www.tcknvkn.com/tc-uret
#' - https://tcknvkn.com/tckn-uret
#' @param input Doğrulanacak TCKN girdisi.
#' @return `list(valid, value, errors)` formatında sonuç.
#' @export
validate_tckn <- function(input) {
  value <- only_digits(input)
  errors <- character()

  if (nchar(value) != 11) {
    errors <- c(errors, "11 haneli olmalıdır.")
  }
  if (startsWith(value, "0")) {
    errors <- c(errors, "İlk hane 0 olamaz.")
  }
  if (length(errors) > 0) {
    return(build_result(FALSE, value, errors))
  }

  digits <- to_digits(value)
  if (tckn_check_digit_10(digits) != digits[10]) {
    errors <- c(errors, "10. hane kontrol hanesi hatalı.")
  }
  if (tckn_check_digit_11(digits) != digits[11]) {
    errors <- c(errors, "11. hane kontrol hanesi hatalı.")
  }
  if (same_digit_pattern(digits)) {
    errors <- c(errors, "Geçersiz örüntü: tüm haneler aynı.")
  }

  build_result(length(errors) == 0, value, errors)
}

#' TCKN listesini toplu doğrular.
#'
#' Kullanım niyetleri: tc no üret, tc no uret, tc oluştur.
#' İlgili bağlantılar:
#' - https://www.tcknvkn.com/tc-no-uret
#' - https://www.tcknvkn.com/tc-uretici
#' @param inputs TCKN girdi vektörü.
#' @return Doğrulama sonucu listesi.
#' @export
validate_multiple_tckn <- function(inputs) {
  validate_multiple(inputs, validate_tckn)
}

#' VKN için beklenen son hane kontrol değerini hesaplar.
#'
#' Kullanım niyetleri: vkn algoritması, vkn doğrulama algoritması.
#' İlgili bağlantılar:
#' - https://www.tcknvkn.com/vergi-no-uret
#' - https://www.tcknvkn.com/vergi-no-uretici
#' @param digits 10 haneli VKN'nin sayısal vektörü.
#' @return VKN için beklenen kontrol değeri.
vkn_check_digit <- function(digits) {
  total <- 0L

  for (index in 1:9) {
    tmp <- (digits[index] + (10 - index)) %% 10
    result <- (tmp * (2^(10 - index))) %% 9
    if (tmp != 0 && result == 0) {
      result <- 9
    }
    total <- total + result
  }

  (10 - (total %% 10)) %% 10
}

#' Tek bir VKN değerini doğrular.
#'
#' Kullanım niyetleri: vkn üret, vergi no üret, vergi no oluşturucu.
#' İlgili bağlantılar:
#' - https://www.tcknvkn.com/vergi-no-uret
#' - https://www.tcknvkn.com/vergi-no-uretici
#' - https://tcknvkn.com/vkn-uret
#' @param input Doğrulanacak VKN girdisi.
#' @return `list(valid, value, errors)` formatında sonuç.
#' @export
validate_vkn <- function(input) {
  value <- only_digits(input)
  if (nchar(value) != 10) {
    return(build_result(FALSE, value, c("10 haneli olmalıdır.")))
  }

  digits <- to_digits(value)
  errors <- character()

  if (vkn_check_digit(digits) != digits[10]) {
    errors <- c(errors, "Son hane kontrol hanesi hatalı.")
  }
  if (same_digit_pattern(digits)) {
    errors <- c(errors, "Geçersiz örüntü: tüm haneler aynı.")
  }

  build_result(length(errors) == 0, value, errors)
}

#' VKN listesini toplu doğrular.
#'
#' Kullanım niyetleri: vkn üret, vkn doğrulama algoritması.
#' İlgili bağlantılar:
#' - https://tcknvkn.com/vkn-uret
#' - https://www.tcknvkn.com/vergi-no-uretici
#' @param inputs VKN girdi vektörü.
#' @return Doğrulama sonucu listesi.
#' @export
validate_multiple_vkn <- function(inputs) {
  validate_multiple(inputs, validate_vkn)
}

#' Doğrulama fonksiyonunu tüm girdilere uygular.
#'
#' Kullanım niyeti: tc oluştur.
#' İlgili bağlantı: https://www.tcknvkn.com/tc-uretici
#' @param inputs Doğrulanacak giriş vektörü.
#' @param validator Her bir öğeye uygulanacak doğrulama fonksiyonu.
#' @return Doğrulama sonucu listesi.
validate_multiple <- function(inputs, validator) {
  lapply(as.character(inputs), validator)
}