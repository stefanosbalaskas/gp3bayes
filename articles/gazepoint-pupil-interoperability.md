# Gazepoint pupil-data interoperability

## Verified Open Gaze fields

The Gazepoint bridge recognizes documented Open Gaze API fields and
reports a mapping proposal. It does not select a left, right,
pixel-diameter, or 3-D pupil channel on the analyst’s behalf.

The API distinguishes camera-image pupil diameter (`LPD`, `RPD`) in
**pixels** from 3-D pupil diameter (`LPUPILD`, `RPUPILD`) in **metres**.
The bridge keeps those units separate.

``` r

gp <- data.frame(
  TIME = seq(0, 0.3, by = 0.1),
  LPD = c(15.1, 15.2, 15.3, 15.2),
  LPV = 1,
  RPD = c(15.0, 15.1, 15.2, 15.1),
  RPV = 1,
  BPOGX = c(.48, .49, .50, .51),
  BPOGY = c(.52, .51, .50, .49),
  BPOGV = 1
)
schema <- inspect_gazepoint_pupil_schema(gp)
schema
#> <gp3bayes_gazepoint_pupil_schema>
#>   Status: ambiguous_pupil_channel
#>   Documented fields detected: 8
#>   Pupil candidates: 2
#>   Candidates: LPD, RPD
#>   Automatic channel selection: FALSE
gazepoint_pupil_mapping_table(schema)
#>        field                        role              unit      eye
#> 1       TIME                        time           seconds     none
#> 2  TIME_TICK                   time_tick             ticks     none
#> 3      LPOGX                 left_gaze_x normalized_screen     left
#> 4      LPOGY                 left_gaze_y normalized_screen     left
#> 5      LPOGV             left_gaze_valid normalized_screen     left
#> 6      RPOGX                right_gaze_x normalized_screen    right
#> 7      RPOGY                right_gaze_y normalized_screen    right
#> 8      RPOGV            right_gaze_valid normalized_screen    right
#> 9      BPOGX                 best_gaze_x normalized_screen combined
#> 10     BPOGY                 best_gaze_y normalized_screen combined
#> 11     BPOGV             best_gaze_valid normalized_screen combined
#> 12      LPCX         left_pupil_camera_x     camera_pixels     left
#> 13      LPCY         left_pupil_camera_y     camera_pixels     left
#> 14       LPD  left_pupil_diameter_pixels            pixels     left
#> 15       LPS            left_pupil_scale             scale     left
#> 16       LPV            left_pupil_valid         indicator     left
#> 17      RPCX        right_pupil_camera_x     camera_pixels    right
#> 18      RPCY        right_pupil_camera_y     camera_pixels    right
#> 19       RPD right_pupil_diameter_pixels            pixels    right
#> 20       RPS           right_pupil_scale             scale    right
#> 21       RPV           right_pupil_valid         indicator    right
#> 22     LEYEX                  left_eye_x            metres     left
#> 23     LEYEY                  left_eye_y            metres     left
#> 24     LEYEZ                  left_eye_z            metres     left
#> 25   LPUPILD  left_pupil_diameter_metres            metres     left
#> 26   LPUPILV         left_pupil_3d_valid         indicator     left
#> 27     REYEX                 right_eye_x            metres    right
#> 28     REYEY                 right_eye_y            metres    right
#> 29     REYEZ                 right_eye_z            metres    right
#> 30   RPUPILD right_pupil_diameter_metres            metres    right
#> 31   RPUPILV        right_pupil_3d_valid         indicator    right
#>                                  source_specification present
#> 1  Gazepoint Open Gaze API v2-era field specification    TRUE
#> 2  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 3  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 4  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 5  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 6  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 7  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 8  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 9  Gazepoint Open Gaze API v2-era field specification    TRUE
#> 10 Gazepoint Open Gaze API v2-era field specification    TRUE
#> 11 Gazepoint Open Gaze API v2-era field specification    TRUE
#> 12 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 13 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 14 Gazepoint Open Gaze API v2-era field specification    TRUE
#> 15 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 16 Gazepoint Open Gaze API v2-era field specification    TRUE
#> 17 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 18 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 19 Gazepoint Open Gaze API v2-era field specification    TRUE
#> 20 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 21 Gazepoint Open Gaze API v2-era field specification    TRUE
#> 22 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 23 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 24 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 25 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 26 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 27 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 28 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 29 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 30 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 31 Gazepoint Open Gaze API v2-era field specification   FALSE
```

Because both left and right pupil channels are present, channel
selection is ambiguous and must be explicit.

## 3-D pupil diameter

``` r

gp3d <- data.frame(
  TIME = c(0, .1, .2),
  LPUPILD = c(.0031, .0032, .0033),
  LPUPILV = 1,
  LEYEX = c(-.04, -.04, -.04),
  LEYEY = c(0, 0, 0),
  LEYEZ = c(.65, .65, .65)
)
gazepoint_pupil_mapping_table(inspect_gazepoint_pupil_schema(gp3d))
#>        field                        role              unit      eye
#> 1       TIME                        time           seconds     none
#> 2  TIME_TICK                   time_tick             ticks     none
#> 3      LPOGX                 left_gaze_x normalized_screen     left
#> 4      LPOGY                 left_gaze_y normalized_screen     left
#> 5      LPOGV             left_gaze_valid normalized_screen     left
#> 6      RPOGX                right_gaze_x normalized_screen    right
#> 7      RPOGY                right_gaze_y normalized_screen    right
#> 8      RPOGV            right_gaze_valid normalized_screen    right
#> 9      BPOGX                 best_gaze_x normalized_screen combined
#> 10     BPOGY                 best_gaze_y normalized_screen combined
#> 11     BPOGV             best_gaze_valid normalized_screen combined
#> 12      LPCX         left_pupil_camera_x     camera_pixels     left
#> 13      LPCY         left_pupil_camera_y     camera_pixels     left
#> 14       LPD  left_pupil_diameter_pixels            pixels     left
#> 15       LPS            left_pupil_scale             scale     left
#> 16       LPV            left_pupil_valid         indicator     left
#> 17      RPCX        right_pupil_camera_x     camera_pixels    right
#> 18      RPCY        right_pupil_camera_y     camera_pixels    right
#> 19       RPD right_pupil_diameter_pixels            pixels    right
#> 20       RPS           right_pupil_scale             scale    right
#> 21       RPV           right_pupil_valid         indicator    right
#> 22     LEYEX                  left_eye_x            metres     left
#> 23     LEYEY                  left_eye_y            metres     left
#> 24     LEYEZ                  left_eye_z            metres     left
#> 25   LPUPILD  left_pupil_diameter_metres            metres     left
#> 26   LPUPILV         left_pupil_3d_valid         indicator     left
#> 27     REYEX                 right_eye_x            metres    right
#> 28     REYEY                 right_eye_y            metres    right
#> 29     REYEZ                 right_eye_z            metres    right
#> 30   RPUPILD right_pupil_diameter_metres            metres    right
#> 31   RPUPILV        right_pupil_3d_valid         indicator    right
#>                                  source_specification present
#> 1  Gazepoint Open Gaze API v2-era field specification    TRUE
#> 2  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 3  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 4  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 5  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 6  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 7  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 8  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 9  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 10 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 11 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 12 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 13 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 14 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 15 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 16 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 17 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 18 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 19 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 20 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 21 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 22 Gazepoint Open Gaze API v2-era field specification    TRUE
#> 23 Gazepoint Open Gaze API v2-era field specification    TRUE
#> 24 Gazepoint Open Gaze API v2-era field specification    TRUE
#> 25 Gazepoint Open Gaze API v2-era field specification    TRUE
#> 26 Gazepoint Open Gaze API v2-era field specification    TRUE
#> 27 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 28 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 29 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 30 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 31 Gazepoint Open Gaze API v2-era field specification   FALSE
```

A proposed schema is an interoperability audit, not evidence that a
column is appropriate for a specific scientific analysis. Export
variants and preprocessing provenance remain part of the contract.
