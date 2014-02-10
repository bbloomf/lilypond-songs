\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -4)
       (stretchability . 100))
  top-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -2)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #129
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key g \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8 d8 |
  g8.[ g16] g8 d'4 c8 |
  b4 g8 g[ fis] g |
  a[ a] a d[ d] c |
  b4 b8\rest b4\rest \bar"" d,16[ d] |
  g4 g8 d'4 c8 |
  b4 g8 g fis g |
  a[ a] a d,[ e] fis |
  g4 b8\rest b4\rest \bar"||"
  b8 |
  c4 b8 a4 g8 |
  fis4 e8 d4 b'16[ b] |
  c4 b16[ b] d4 b8 |
  a4 b8\rest b4\rest \bar"" a16[ a] |

  b8[ c] a g fis g |
  a b c b4 g16[ g] |
  fis8 e fis d e fis |
  g4 g8 a4 a8 |

  b4 b8 c4\fermata \bar"" g16[ g] |
  fis8 e fis d e fis |
  g4 b8\rest b4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	\set ignoreMelismata = ##t
  The Lords of cre -- a -- tion men we call,
  And they think _ they rule _ the whole;
  But they’re much mis -- tak -- en af -- ter all,
  For they’re un -- _ der wom -- an’s con -- trol.

  As ev -- er since the world be -- gan,
  It has al -- ways __ _ been the way,
  For __ _ did __ _ not A -- dam, the ver -- y first man,
  The __ _ ver -- y first wo -- man o -- bey, o -- bey, o -- bey, o -- bey?
  The __ _ ver -- y first wo -- man o -- bey!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  Ye Lords, who at pres -- ent hear my song, __ _
  I know _ you’ll quick -- _ ly say:
  “Our __ _ size more large, our nerves more strong;
  Shall the strong -- er the weak -- er o -- bey?”

  But think not though these words we hear
  We shall e’er mind the thing you say;
  For as long as a wo -- man’s pos -- sessed of a tear,
  Your _ pow -- er will van -- ish a -- way, a -- way, a -- way, a -- way;
  Your _ pow -- er will van -- ish a -- way.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  But should _ there be so strange a wight, _
  As not to be moved by a tear,
  Though _ much as -- ton -- ished at the sight
  We shall still have no cause _ for fear.

  Then let them please them -- selves a -- while
  Up -- _ on their __ _ fan -- cied sway,
  For as long as a wo -- man’s pos -- sessed of a smile
  She will cer -- tain -- ly have her own way, her way, her way, ah! yes,
  She’ll _ cer -- tain -- ly have her own way.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  Now, La -- _ dies since I’ve made it plain
  That the thing _ is real -- _ ly so,
  We’ll __ _ e -- ven let them hold the rein,
  But we’ll show them the way __ _ to go;

  As ev -- er since the world be -- gan
  It has al -- ways __ _ been the way,
  And we’ll man -- age it so that the ver -- y last man
  Shall the ver -- y last wo -- man o -- bey, o -- bey, o -- bey, o -- bey;
  Shall the ver -- y last wo -- man o -- bey.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d8 |
  d8.[ d16] d8 g4 fis8 |
  g4 d8 d[ d] d |
  fis[ fis] fis fis[ fis] fis |
  g4 s4.

  d16[ d] |
  d4 d8 g4 fis8 |
  g4 d8 d d d |
  fis[ fis] fis d[ e] c |
  b4 s4. \bar"||"

  d8 |
  e4 d8 fis4 g8 |
  d4 c8 d4 d16[ d] |
  e4 d16[ d] g4 g8 |
  fis4 s4. fis16[ fis] |

  g8[ e] fis g e e |
  fis fis fis g4 e16[ e] |
  d8 c d d c d |
  d4 d8 fis4 fis8 |
  g4 g8 e4 e16[ e] |
  d8 c d d e c |
  b4 s4. \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  d,8 |
  b'8.[ b16] g8 b4 a8 |
  g4  b8 b[ a] b |
  c[ c] c c[ b] a |
  g4 s4.

  d16[ d] |
  b'4 g8 b4 a8 |
  g4 b8 b a b |
  c[ c] c fis,[ g] a |
  g4 s4. \bar"||"

  g8 |
  g4 g8 c4 b8 |
  a4 g8 fis4 g16[ g] |
  g4 g16[ g] b4 g8 |
  a4 s4. a16[ a] |

  g8[ g] a b b b |
  c b a d4 b16[ b] |
  a8 a a fis g a |
  b4 b8 c4 c8 |
  d4 d8 g,4 g16[ g] |
  a8 a a fis g a |
  g4 s4. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  d,8 |
  g,8.[ g16] b8 d4 d8 |
  g,4 g8 g[ d'] d |
  d[ d] d d[ d] d |
  g,4 d'8\rest d4\rest d16[ d] |
  
  g,4 b8 d4 d8 |
  g,4 g8 g d' d |
  d8[ d] d d[ d] d |
  g,4 d'8\rest d4\rest
  g,8 |

  c4 g8 d'4 e8 |
  d4 d8 d4 g,16[ g] |
  c4 g16[ g] g4 g8 |
  d'4 d8\rest d4\rest d16[ d] |

  d8[ c] d e e e |
  d d d g,4 e'16[ e] |
  d8 d d d d d |
  g,4 g8 d'4 d8 |
  g,4 g8 c4\fermata c16[ c] |
  d8 d d d d d |
  g,4 d'8\rest d4\rest \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Lords of Creation"}}
  composer = \markup\oldStyleNum"J.S.R."
  tagline = ""
}}
