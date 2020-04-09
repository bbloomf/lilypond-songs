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
       (padding . 0)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #77
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 18 20))) }
global = {
  \key aes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
  \slurDashed
}

sopMusic = \relative c' {
  \partial 4
  aes'8( bes) |
  c~ c c8. bes16 aes8~ aes f8. g16 |
  aes8 aes aes8. g16 f2 |
  ees4 ees8. des16 c8 ees aes8. bes16 |
  c2 bes4 \bar""
  
  aes8( bes) |
  c~ c c8. bes16 aes8~ aes f8.( g16) |
  aes8~ aes aes8.( g16) f2 |
  ees4 ees8. des16 c8 ees aes8. c16 |
  bes2 aes4 \bar"||"\break
  
  b8\rest ees |
  ees4 c8. des16 ees8 f4 ees8 |
  ees8 ees c8. des16 ees2 |
  ees4 c8. des16 ees8 f4 f8 |
  ees4 c8. aes16 bes4 \bar"" aes8 bes 
  
  c8 c c8. bes16 aes8 aes f8. g16 |
  aes8 aes aes8. g16 f2 |
  ees4 ees8. des16 c8 ees aes8. c16 |
  bes2 aes4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  Our __ _ flag is proud -- ly float -- ing on the land and on the main,
  Shout, shout the bat -- tle cry of Free -- dom!
  Be -- _ neath it oft we’ve con -- quered, and we’ll con -- quer oft a -- gain!
  Shout, shout the bat -- tle cry of Free -- dom!
  
  Our Dix -- ie for -- ev -- er! She’s nev -- er at a loss!
  Down with the ea -- gle and up with the cross
  We -- ’ll ral -- ly ’round the bon -- ny flag, we’ll ral -- ly once a -- gain,
  Shout, shout the bat -- tle cry of Free -- dom!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  Our __ _ gal -- lant boys have marched _ to the rol -- ling of the drums.
  Shout, shout the bat -- tle cry of Free -- dom!
  And the lead -- _ ers in charge cry out, __ _ “Come, _ boys, __ _ come!”
  Shout, shout the bat -- tle cry of Free -- dom!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  They have laid __ _ down their lives __ _ on the blood -- y bat -- tle field.
  Shout, shout the bat -- tle cry of Free -- dom!
  Their _ mot -- to is re -- sis -- tance— “To the ty -- rants nev -- er yield!”
  Shout, shout the bat -- tle cry of Free -- dom!
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  While our boys __ _ have re -- spond -- _ ed and to the fields have gone.
  Shout, shout the bat -- tle cry of Free -- dom!
  Our __ _ no -- ble wo -- men al -- _ so have aid -- ed them at home.
  Shout, shout the bat -- tle cry of Free -- dom!
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  ees8( des) |
  c~ c c8. c16 c8~ c c8. c16 |
  c8 c c8. bes16 aes2 |
  c4 c8. bes16 aes8 c ees8. ees16 |
  ees2 ees4
  
  ees8( des) |
  c~ c c8. c16 c8~ c c8.~ c16 |
  c8~ c c8.( bes16) aes2 |
  c4 c8. bes16 aes8 c ees8. ees16 |
  des2 c4
  
  
  s8 aes' |
  aes4 aes8. aes16 aes8 aes4 aes8 |
  aes8 aes aes8. aes16 aes2 |
  aes4 aes8. aes16 aes8 aes4 aes8 |
  ees4 f8. f16 g4 ees8 des |
  
  c c c8. c16 c8 c c8. c16 |
  c8 c c8. bes16 aes2 |
  c4 c8. bes16 aes8 c ees8. ees16 |
  des2 c4
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
  aes8( g) |
  aes~ aes aes8. aes16 aes8~ aes aes8. g16 |
  f8 f e8. e16 f2 |
  aes4 aes8. aes16 aes8 aes aes8. g16 |
  aes2 g4 
  
  aes8( g) |
  aes~ aes aes8. aes16 aes8~ aes aes8.( g16) |
  f8~ f e8.~ e16 f2 |
  aes4 aes8. aes16 aes8 aes aes8. aes16 |
  g2 aes4 
  
  
  s8 c8 |
  c4 aes8. bes16 c8 des4 c8 |
  c8 c aes8. bes16 c2 |
  c4 aes8. bes16 c8 des4 des8 |
  c4 aes8. aes16 g4 aes8 g |
  
  aes aes aes8. aes16 aes8 aes aes8. g16 |
  f8 f e8. e16 f2 |
  aes4 aes8. aes16 aes8 aes aes8. aes16 |
  g2 aes4 
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  c,8( bes) |
  aes~ aes aes8. aes16 f8~ f f8. f16 |
  c'8 c c8. c16 des2 |
  aes4 aes8. aes16 aes8 aes c8. ees16 |
  aes,2 ees'4 
  
  c8( bes) |
  aes~ aes aes8. aes16 f8~ f f8.~ f16 |
  c'8~ c c8.~ c16 des2 |
  aes4 aes8. aes16 aes8 aes c8. c16 |
  ees2 aes,4 
  
  
  d8\rest aes' |
  aes4 aes8. aes16 aes8 aes4 aes8 |
  aes8 aes aes8. aes16 aes2 |
  aes4 aes8. aes16 aes8 aes4 aes8 |
  aes4 f8. f16 ees8[ des] c8 bes |
  
  aes aes aes8. aes16 f8 f f8. f16 |
  c'8 c c8. c16 des2 |
  aes4 aes8. aes16 aes8 aes c8. c16 |
  ees2 aes,4 
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
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"The Battle Cry of Freedom"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #12.5 \smallCapsOldStyle"(Confederate Version)"}}
  composer = \markup\oldStyleNum"George Frederick Root (1825–1895)"
  poet = \markup\oldStyleNum"W. H. Barnes"
  tagline = ""
}}
