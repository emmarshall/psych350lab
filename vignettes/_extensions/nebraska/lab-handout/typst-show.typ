#show: project.with(
$if(title)$
  title: "$title$",
$endif$
$if(subtitle)$
  subtitle: "$subtitle$",
$endif$
$if(logo-path)$
  logo: "$logo-path$",
$endif$
$if(logo-alt)$
  logo-alt: "$logo-alt$",
$endif$
$if(lab)$
  lab: "$lab$",
$endif$
$if(dataset)$
  dataset: "$dataset$",
$endif$
$if(course)$
  course: "$course$",
$endif$
$if(instructor)$
  instructor: "$instructor$",
$endif$
$if(due-date)$
  due-date: "$due-date$",
$endif$
)
