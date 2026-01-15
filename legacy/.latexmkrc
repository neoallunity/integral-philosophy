# .latexmkrc -- Unified configuration for Journal project

# Use LuaLaTeX for PDF compilation with necessary flags  -lualatex -f -g -bibtex -deps -synctex=1 -interaction=nonstopmode -output-directory=tmp
$pdflatex = 'lualatex -interaction=nonstopmode --shell-escape --file-line-error --synctex=1 %O %S';

# Use Biber for bibliography
$biber = 'biber %O %B';
# Keep bibtex command in case it's needed by some packages (though biber is primary)
$bibtex = 'bibtex %O %S';

# Set output and auxiliary directories
$out_dir = 'tmp';
$aux_dir = 'tmp';

# Use PDF mode
$pdf_mode = 1;

# Disable continuous preview (to prevent compilation pausing for input)
$preview_continuous_mode = 0;
# Also ensure external viewers don't block
$preview_mode = 0;

# Enable deletion of *.bbl when calling "latexmk -c"
$bibtex_use = 2;

# --- Glossaries (glossaries-extra) ---
# Use makeglossaries-lite with silent option for robustness
add_cus_dep('glo', 'gls', 0, 'run_makeglossaries');
add_cus_dep('acn', 'acr', 0, 'run_makeglossaries');
sub run_makeglossaries {
  my ($base) = @_;
  # Use makeglossaries-lite if available, otherwise fallback to makeglossaries
  if (system("command -v makeglossaries-lite >/dev/null 2>&1") == 0) {
    return system("makeglossaries-lite", "-q", "$base");
  } else {
    return system("makeglossaries", "$base");
  }
}

# --- Indexes (imakeidx: subject/names/geo etc.) ---
# latexmk already knows how to run makeindex for .idx -> .ind; we keep defaults.
# This line makes sure the standard makeindex command is used for every .idx it finds.
$makeindex = 'makeindex %O -o %D %S';

# Remove more files than in the default configuration
# Removed 'bbl' from generated_exts as $bibtex_use=2 handles it, but keeps other important clean files.
@generated_exts = qw(acn acr alg aux code fls glg glo gls glsdefs idx ind ist lof lol lot out run.xml synctex thm toc tpt upa upb wrt xdy);
$clean_ext .= ' %R.ist %R.xdy';

