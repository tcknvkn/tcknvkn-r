# -----------------------------------------------------------------------------
# Proje: tcknvkn-r
# Dosya: tests/test_tcknvkn.R
# Açıklama: TCKN ve VKN doğrulama fonksiyonları için birim test senaryolarını içerir.
# Oluşturma Tarihi: 2026-04-24
# Lisans: MIT
# Site: https://www.tcknvkn.com
# -----------------------------------------------------------------------------
source("R/tcknvkn.R", local = TRUE)

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

assert_equal <- function(actual, expected, message) {
  if (!identical(actual, expected)) {
    expected_text <- paste(deparse(expected), collapse = "")
    actual_text <- paste(deparse(actual), collapse = "")
    stop(sprintf("%s\nExpected: %s\nActual: %s", message, expected_text, actual_text), call. = FALSE)
  }
}

# TCKN tekil senaryoları
res <- validate_tckn("10000000146")
assert_true(res$valid, "Geçerli TCKN doğrulanamadı")
assert_equal(res$value, "10000000146", "Geçerli TCKN value alanı hatalı")
assert_equal(length(res$errors), 0L, "Geçerli TCKN için hata dönmemeliydi")

res <- validate_tckn("100-000 00146")
assert_true(res$valid, "Normalize edilmiş TCKN doğrulanamadı")
assert_equal(res$value, "10000000146", "Normalize edilmiş TCKN value alanı hatalı")

res <- validate_tckn("12345")
assert_true(!res$valid, "Eksik haneli TCKN geçersiz olmalı")
assert_true("11 haneli olmalıdır." %in% res$errors, "11 hane hatası bekleniyordu")

res <- validate_tckn("01234567890")
assert_true(!res$valid, "0 ile başlayan TCKN geçersiz olmalı")
assert_true("İlk hane 0 olamaz." %in% res$errors, "İlk hane hatası bekleniyordu")

res <- validate_tckn("10000000156")
assert_true(!res$valid, "10. hane hatalı TCKN geçersiz olmalı")
assert_true("10. hane kontrol hanesi hatalı." %in% res$errors, "10. hane hatası bekleniyordu")

res <- validate_tckn("10000000145")
assert_true(!res$valid, "11. hane hatalı TCKN geçersiz olmalı")
assert_true("11. hane kontrol hanesi hatalı." %in% res$errors, "11. hane hatası bekleniyordu")

res <- validate_tckn("11111111111")
assert_true(!res$valid, "Tekrarlı TCKN geçersiz olmalı")
assert_true("Geçersiz örüntü: tüm haneler aynı." %in% res$errors, "Örüntü hatası bekleniyordu")

# VKN tekil senaryoları
res <- validate_vkn("1000036109")
assert_true(res$valid, "Geçerli VKN doğrulanamadı")
assert_equal(res$value, "1000036109", "Geçerli VKN value alanı hatalı")
assert_equal(length(res$errors), 0L, "Geçerli VKN için hata dönmemeliydi")

res <- validate_vkn("100-003-6109")
assert_true(res$valid, "Normalize edilmiş VKN doğrulanamadı")
assert_equal(res$value, "1000036109", "Normalize edilmiş VKN value alanı hatalı")

res <- validate_vkn("1234")
assert_true(!res$valid, "Eksik haneli VKN geçersiz olmalı")
assert_true("10 haneli olmalıdır." %in% res$errors, "10 hane hatası bekleniyordu")

res <- validate_vkn("1000036108")
assert_true(!res$valid, "Checksum hatalı VKN geçersiz olmalı")
assert_true("Son hane kontrol hanesi hatalı." %in% res$errors, "Checksum hatası bekleniyordu")

res <- validate_vkn("1111111111")
assert_true(!res$valid, "Tekrarlı VKN geçersiz olmalı")
assert_true("Geçersiz örüntü: tüm haneler aynı." %in% res$errors, "Örüntü hatası bekleniyordu")

# Toplu doğrulama sırası
batch_tckn <- validate_multiple_tckn(c("10000000146", "10000000145", "11111111111", "100-000 00146"))
assert_equal(length(batch_tckn), 4L, "Toplu TCKN uzunluğu hatalı")
assert_true(batch_tckn[[1]]$valid, "Toplu TCKN ilk kayıt geçerli olmalı")
assert_true(!batch_tckn[[2]]$valid, "Toplu TCKN ikinci kayıt geçersiz olmalı")
assert_true(!batch_tckn[[3]]$valid, "Toplu TCKN üçüncü kayıt geçersiz olmalı")
assert_true(batch_tckn[[4]]$valid, "Toplu TCKN dördüncü kayıt geçerli olmalı")

batch_vkn <- validate_multiple_vkn(c("1000036109", "1000036108", "1111111111", "100-003-6109"))
assert_equal(length(batch_vkn), 4L, "Toplu VKN uzunluğu hatalı")
assert_true(batch_vkn[[1]]$valid, "Toplu VKN ilk kayıt geçerli olmalı")
assert_true(!batch_vkn[[2]]$valid, "Toplu VKN ikinci kayıt geçersiz olmalı")
assert_true(!batch_vkn[[3]]$valid, "Toplu VKN üçüncü kayıt geçersiz olmalı")
assert_true(batch_vkn[[4]]$valid, "Toplu VKN dördüncü kayıt geçerli olmalı")

cat("All tests passed\n")