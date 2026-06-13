# _plugins/r_extras_lexer.rb
#
# A Rouge lexer for R with tidyverse + time-series / modelling vocabulary highlighted.
# Drop this file into your Jekyll site's _plugins/ directory.
#
# IMPORTANT:
# - Remove/rename any other custom R lexers that also register `tag 'r'`
#   (e.g. r_tidyverse_lexer.rb), otherwise only one may take effect.
# - This lexer highlights selected package vocabulary as Name::Function.
#
# Covered packages:
#   dplyr, tidyr, purrr, tibble, readr, stringr, forcats, lubridate,
#   ggplot2, janitor, rlang, glue,
#   forecast, stats, mgcv, boot, ggrepel, seasonal, fable, tsibble
#
# Notes:
# - Pipe operators like %>% are highlighted as Operator.
# - Short mgcv helpers s(), te(), ti(), t2() are only highlighted when
#   followed by "(" to avoid false positives.
# - Most function names are also only highlighted when followed by "(".
#   This avoids over-highlighting variable names like `time` or `forecast`.

require 'rouge'

module Rouge
  module Lexers
    class RCombined < R
      title     'R'
      desc      'R with tidyverse, forecasting, time-series, and modelling vocabulary highlighted'
      tag       'r'
      aliases   'R', 'rscript'
      filenames '*.r', '*.R'
      mimetypes 'text/x-r-source', 'text/x-r', 'text/x-R'

      # ------------------------------------------------------------------
      # dplyr
      # ------------------------------------------------------------------
      DPLYR = %w[
        mutate mutate_if mutate_at mutate_all
        transmute transmute_if transmute_at transmute_all
        filter filter_if filter_at filter_all
        select select_if select_at select_all
        rename rename_with rename_if rename_at rename_all
        relocate
        arrange arrange_if arrange_at arrange_all desc
        group_by group_by_if group_by_at group_by_all
        ungroup regroup
        summarise summarise_if summarise_at summarise_all
        summarize summarize_if summarize_at summarize_all
        count tally add_count add_tally
        distinct n_distinct
        slice slice_head slice_tail slice_min slice_max slice_sample
        pull glimpse
        between near coalesce na_if
        case_when if_else
        first last nth
        lead lag
        row_number min_rank dense_rank percent_rank cume_dist ntile
        n
        across c_across where everything
        starts_with ends_with contains matches num_range last_col all_of any_of
        bind_rows bind_cols
        left_join right_join inner_join full_join semi_join anti_join cross_join
        join_by
        do
      ].freeze

      # ------------------------------------------------------------------
      # tidyr
      # ------------------------------------------------------------------
      TIDYR = %w[
        pivot_longer pivot_wider
        gather spread
        nest unnest unnest_longer unnest_wider
        hoist
        separate separate_rows separate_wider_delim
        separate_wider_position separate_wider_regex
        unite
        fill complete drop_na replace_na
        expand expand_grid crossing nesting
        chop unchop pack unpack
      ].freeze

      # ------------------------------------------------------------------
      # purrr
      # ------------------------------------------------------------------
      PURRR = %w[
        map map2 pmap imap lmap
        map_lgl map_int map_dbl map_chr map_raw map_vec
        map2_lgl map2_int map2_dbl map2_chr map2_raw map2_vec
        pmap_lgl pmap_int pmap_dbl pmap_chr pmap_raw pmap_vec
        walk walk2 pwalk iwalk
        map_df map_dfr map_dfc
        modify modify_if modify_at modify2 imodify modify_in
        reduce reduce2 reduce_right accumulate accumulate2
        keep keep_at discard discard_at compact
        every some none detect detect_index
        flatten flatten_lgl flatten_int flatten_dbl flatten_chr
        flatten_dfr flatten_dfc
        transpose
        safely quietly possibly auto_browse
        negate partial compose
        list_rbind list_cbind list_flatten list_transpose list_assign
        pluck pluck_exists chuck pluck_depth
        set_names
      ].freeze

      # ------------------------------------------------------------------
      # tibble
      # ------------------------------------------------------------------
      TIBBLE = %w[
        tibble tribble as_tibble is_tibble
        add_row add_column
        enframe deframe
        new_tibble
      ].freeze

      # ------------------------------------------------------------------
      # readr
      # ------------------------------------------------------------------
      READR = %w[
        read_csv read_csv2 read_tsv read_delim read_fwf read_table
        read_log read_lines read_file read_rds
        write_csv write_csv2 write_tsv write_delim
        write_lines write_file write_rds
        parse_number parse_double parse_integer parse_logical
        parse_character parse_factor parse_date parse_datetime parse_time
        col_types cols col_character col_double col_integer col_logical
        col_date col_datetime col_time col_factor col_skip col_guess
        spec_csv spec_delim problems
      ].freeze

      # ------------------------------------------------------------------
      # stringr
      # ------------------------------------------------------------------
      STRINGR = %w[
        str_c str_glue str_glue_data
        str_length str_width str_pad str_trunc str_trim str_squish
        str_to_upper str_to_lower str_to_title str_to_sentence
        str_sub str_sub_all
        str_detect str_starts str_ends str_which str_count
        str_locate str_locate_all
        str_extract str_extract_all str_match str_match_all
        str_replace str_replace_all str_remove str_remove_all
        str_split str_split_fixed str_split_1 str_split_i
        str_flatten str_flatten_comma
        str_sort str_order str_unique str_duplicated
        str_wrap str_indent
        str_view str_view_all
        str_conv str_encoding str_escape str_interp str_dup
        fixed regex perl boundary coll
      ].freeze

      # ------------------------------------------------------------------
      # forcats
      # ------------------------------------------------------------------
      FORCATS = %w[
        fct_relevel fct_reorder fct_reorder2 fct_inorder fct_infreq fct_inseq
        fct_rev fct_shuffle fct_shift fct_relabel
        fct_collapse fct_lump fct_lump_min fct_lump_n
        fct_lump_prop fct_lump_lowfreq
        fct_other fct_recode fct_drop fct_explicit_na
        fct_na_value_to_level fct_anon fct_cross
        fct_count fct_match fct_unique
        as_factor
        lvls_reorder lvls_revalue lvls_union
      ].freeze

      # ------------------------------------------------------------------
      # lubridate
      # ------------------------------------------------------------------
      LUBRIDATE = %w[
        ymd mdy dmy ydm myd dym
        ymd_hms ymd_hm ymd_h mdy_hms mdy_hm mdy_h
        dmy_hms dmy_hm dmy_h
        year month week day hour minute second
        yday mday wday qday
        isoyear isoweek
        date_decimal decimal_date
        now today
        as_date as_datetime
        make_date make_datetime
        floor_date ceiling_date round_date
        rollback rollforward
        days_in_month leap_year
        period duration interval
        seconds minutes hours days weeks months years
        seconds_to_period period_to_seconds
        with_tz force_tz am pm
        parse_date_time fast_strptime
        stamp stamp_date stamp_time
      ].freeze

      # ------------------------------------------------------------------
      # ggplot2
      # ------------------------------------------------------------------
      GGPLOT2 = %w[
        ggplot aes aes_string vars
        geom_point geom_line geom_bar geom_col geom_histogram geom_density
        geom_boxplot geom_violin geom_jitter geom_dotplot
        geom_smooth geom_abline geom_hline geom_vline geom_segment geom_curve
        geom_ribbon geom_area geom_polygon geom_map geom_rug
        geom_tile geom_raster geom_rect
        geom_text geom_label
        geom_contour geom_contour_filled geom_density_2d geom_density_2d_filled
        geom_hex geom_bin_2d geom_count
        geom_crossbar geom_errorbar geom_errorbarh geom_linerange geom_pointrange
        geom_path geom_step geom_spoke
        geom_sf geom_sf_text geom_sf_label
        stat_smooth stat_summary stat_summary_2d stat_summary_hex
        stat_bin stat_bin_2d stat_density stat_density_2d
        stat_count stat_ecdf stat_ellipse stat_function
        stat_identity stat_qq stat_qq_line stat_unique
        scale_x_continuous scale_y_continuous
        scale_x_discrete scale_y_discrete
        scale_x_log10 scale_y_log10 scale_x_sqrt scale_y_sqrt
        scale_x_reverse scale_y_reverse
        scale_x_date scale_y_date scale_x_datetime scale_y_datetime
        scale_colour_continuous scale_color_continuous
        scale_colour_discrete scale_color_discrete
        scale_colour_manual scale_color_manual
        scale_colour_brewer scale_color_brewer
        scale_colour_distiller scale_color_distiller
        scale_colour_viridis_c scale_color_viridis_c
        scale_colour_viridis_d scale_color_viridis_d
        scale_fill_continuous scale_fill_discrete scale_fill_manual
        scale_fill_brewer scale_fill_distiller
        scale_fill_viridis_c scale_fill_viridis_d
        scale_size scale_size_continuous scale_size_discrete
        scale_size_manual scale_size_area
        scale_alpha scale_alpha_continuous scale_alpha_discrete scale_alpha_manual
        scale_linetype scale_linetype_manual scale_shape scale_shape_manual
        facet_wrap facet_grid
        coord_cartesian coord_flip coord_fixed coord_equal
        coord_polar coord_map coord_quickmap coord_sf coord_trans
        theme theme_set theme_get theme_update theme_replace
        theme_grey theme_gray theme_bw theme_linedraw theme_light theme_dark
        theme_minimal theme_classic theme_void theme_test
        element_text element_line element_rect element_blank element_grob
        margin unit rel
        labs xlab ylab ggtitle xlim ylim lims
        guides guide_legend guide_colourbar guide_colorbar
        guide_bins guide_coloursteps guide_none
        annotation_custom annotation_logticks annotation_map annotation_raster
        annotate
        position_dodge position_dodge2 position_jitter position_jitterdodge
        position_nudge position_stack position_fill position_identity
        ggsave last_plot expansion
      ].freeze

      # ------------------------------------------------------------------
      # misc
      # ------------------------------------------------------------------
      MISC = %w[
        clean_names data.frame
      ].freeze

      # ------------------------------------------------------------------
      # rlang / tidy eval
      # ------------------------------------------------------------------
      RLANG = %w[
        enquo enquos quo quos
        enexpr enexprs expr exprs
        sym syms ensym ensyms
        eval_tidy eval_bare
        as_name as_label as_string
        is_quosure is_symbolic
        quo_is_null quo_is_symbol quo_is_call
        local_options with_options
        abort warn inform
        is_true is_false is_null is_na
        is_character is_double is_integer is_logical is_numeric is_list
        is_function is_closure is_primitive
        is_empty
        new_function lambda
        exec inject zap
      ].freeze

      # ------------------------------------------------------------------
      # glue
      # ------------------------------------------------------------------
      GLUE = %w[
        glue glue_data glue_collapse glue_sql glue_data_sql glue_safe
      ].freeze

      # ------------------------------------------------------------------
      # forecast
      # ------------------------------------------------------------------
      FORECAST = %w[
        forecast
        auto.arima Arima arimaorder
        ets bats tbats
        nnetar tslm thetaf croston
        meanf naive snaive rwf
        msts fourier seasonplot
        checkresiduals accuracy residuals
        ndiffs nsdiffs
        BoxCox InvBoxCox BoxCox.lambda
        dm.test
      ].freeze

      # ------------------------------------------------------------------
      # stats
      # curated subset
      # ------------------------------------------------------------------
      STATS = %w[
        ts window start end frequency deltat time cycle lag
        aggregate ts.intersect ts.union
        stl decompose spec.ar
        acf pacf ccf
        arima arima.sim makeARIMA
        HoltWinters
        lm glm nls loess
        predict fitted residuals coef vcov
        model.frame model.matrix terms formula update
        tsdiag
        optim optimize nlm
        spline smooth.spline approx approxfun
        density ecdf
        dist hclust kmeans
        cor cov var sd median quantile IQR mad
        rnorm dnorm pnorm qnorm
        runif dunif punif qunif
        rpois dpois ppois qpois
        rgamma dgamma pgamma qgamma
        rbinom dbinom pbinom qbinom
        rexp dexp pexp qexp
        t.test wilcox.test ks.test chisq.test fisher.test
        shapiro.test binom.test prop.test cor.test
        anova aov TukeyHSD
        p.adjust p.adjust.methods
        poly smooth embed
      ].freeze

      # ------------------------------------------------------------------
      # mgcv
      # ------------------------------------------------------------------
      MGCV = %w[
        gam bam gamm
        gam.check choose.k concurvity
        vis.gam plot.gam predict.gam anova.gam
        smoothCon linear.functional.terms
      ].freeze

      MGCV_SHORT = %w[
        s te ti t2
      ].freeze

      # ------------------------------------------------------------------
      # boot
      # ------------------------------------------------------------------
      BOOT = %w[
        boot boot.ci tsboot censboot
        cv.glm jack.after.boot boot.array
        envelope empinf
        tilt.boot saddle
      ].freeze

      # ------------------------------------------------------------------
      # ggrepel
      # ------------------------------------------------------------------
      GGREPEL = %w[
        geom_text_repel geom_label_repel
        position_nudge_repel
      ].freeze

      # ------------------------------------------------------------------
      # seasonal
      # ------------------------------------------------------------------
      SEASONAL = %w[
        seas final series out static update
        monthplot yearplot view identify
        inspect udg
      ].freeze

      # ------------------------------------------------------------------
      # fable
      # ------------------------------------------------------------------
      FABLE = %w[
        ARIMA ETS TSLM NNETAR THETA CROSTON
        decomposition_model combination_model
        forecast accuracy generate hilo unpack_hilo
        components gg_tsresiduals
      ].freeze

      # ------------------------------------------------------------------
      # tsibble
      # ------------------------------------------------------------------
      TSIBBLE = %w[
        tsibble as_tsibble is_tsibble
        build_tsibble update_tsibble
        index key key_vars index_var
        index_by group_by_key
        filter_index
        interval has_gaps scan_gaps count_gaps fill_gaps
        is_regular is_ordered
        yearweek yearmonth yearquarter
        new_data stretch_tsibble
      ].freeze

      # ------------------------------------------------------------------
      # Combined lists
      # ------------------------------------------------------------------
      ALL_WORDS = (
        DPLYR + TIDYR + PURRR + TIBBLE + READR + STRINGR + FORCATS +
        LUBRIDATE + GGPLOT2 + MISC + RLANG + GLUE +
        FORECAST + STATS + MGCV + BOOT + GGREPEL + SEASONAL +
        FABLE + TSIBBLE
      ).uniq.freeze

      # Pipe operators
      PIPE_OPS = [
        '%>%',
        '%T>%',
        '%<>%',
        '%$%'
      ].freeze

      PIPE_RE       = Regexp.union(PIPE_OPS.map { |op| Regexp.new(Regexp.escape(op)) })
      MGCV_SHORT_RE = /\b(#{Regexp.union(MGCV_SHORT)})\b(?=\s*\()/
      FUNCTION_RE   = /\b(#{Regexp.union(ALL_WORDS)})\b(?=\s*\()/

      prepend :root do
        rule PIPE_RE, Operator
        rule MGCV_SHORT_RE, Name::Function
        rule FUNCTION_RE, Name::Function
      end
    end
  end
end
