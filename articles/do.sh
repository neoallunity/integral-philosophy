
create_article_template() {
local author_code="$1"
local author_name="$2"
local author_name_en="$3"

# Английская версия
cat > "${author_code}.tex" << EOF

% ${author_name_en} - English abstract
\subsection{${author_name_en}}
\label{subsec:${author_code}-en}

{\itshape Abstract text for ${author_name_en} will be placed here...}

\paragraph{Keywords:} {\itshape keyword1, keyword2, keyword3}
EOF

# Русская версия
cat > "${author_code}-ru.tex" << EOF

% ${author_name} - русская статья
\subsection{${author_name}}
\label{subsec:${author_code}-ru}

Текст статьи ${author_name} будет размещен здесь...

\paragraph{Ключевые слова:} {\itshape ключ1, ключ2, ключ3}
EOF
}

authors=(
    "moiseev:Моисеев В.И.:Moiseev V.I."
    "safonov:Сафонов А.А.:Safonov A.A."
    "smolyanova:Смольянова Н.А.:Smolyanova N.A."
    "podzolkova:Подзолкова Н.А.:Podzolkova N.A."
    "krasheninnikov:Крашенинников В.В.:Krasheninnikov V.V."
    "bukharov:Бухаров Ю.Д.:Bukharov Yu. D."
    "lugowska:Луговская Е.Г.:Lugowska H."
    "holkin:Холкин О.М.:Holkin O. M."
)

for author_info in "${authors[@]}"; do
    IFS=':' read -r code name_ru name_en <<< "$author_info"
    create_article_template "$code" "$name_ru" "$name_en"
    echo "  ✓ Созданы статьи для: $name_ru"
done
