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
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #131
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
  \key aes \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  ees4 |
  ees aes c |
  c bes bes |
  bes c bes |
  bes aes aes |
  bes aes f |
  ees4. aes8 c4 |
  c bes aes |
  bes2 ees,4 |
  ees4 aes c |
  
  c bes bes |
  bes c bes |
  bes aes aes |
  bes aes f |
  ees ees aes |
  bes c bes |
  aes2 \bar"||"\break \time 4/4 \partial 4 aes8. bes16 |
  c2. c8. des16 |
  ees2. c8[ bes] |
  
  aes8 aes aes aes aes4 g8 aes |
  bes2. aes8. bes16 |
  c2. bes8. aes16 |
  des2( f4) f |
  ees8 ees ees ees ees4 \acciaccatura bes8 des8. c16 |
  aes2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Though trou -- bles per -- plex you, Dis -- heart -- en and vex you,
  Re -- tard -- ing your pro -- gress in som -- ber ar -- ray;
  To shrink from with ter -- ror Is sure -- ly an er -- ror,
  For where there’s a will there is al -- ways a way.
  
  There’s a way, there’s a way,
  Wher -- ev -- er there’s a will there’s a way,
  There’s a way, there’s a way,
  Wher -- ev -- er there’s a will there’s a way.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  The task may be teas -- ing, The du -- ty un -- pleas -- ing,
  But he who con -- fronts it will soon win the day;
  The fight is half o -- ver When once we dis -- cov -- er
  That where there’s a will there is al -- ways a way.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Mis -- for -- tunes un -- count -- ed Are of -- ten sur -- mount -- ed,
  If on -- ly we quit not the field in dis -- may;
  Then one more en -- deav -- or, Re -- mem -- ber -- ing ev -- er,
  That where there’s a will there is al -- ways a way.
  
  \set associatedVoice = "tenors" ""
  There’s a way,
  there’s a way,
  \repeat unfold 9 \skip1
  there’s a way,
  There’s a way,
  there’s a way,
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  c4 |
  c c ees |
  ees des des |
  des ees des |
  des c ees |
  f f des |
  c4. ees8 ees4 |
  ees ees d |
  ees2 des4 |
  c c ees |
  
  ees des des |
  des ees des |
  des c ees |
  f f des |
  c c ees |
  g g g |
  ees2 \bar"||" \time 4/4 \partial 4 c8. des16 |
  ees2. aes8. bes16 |
  c2. aes8[ g] |
  
  f f f f f4 e8 f |
  g2. c,8. des16 |
  ees2. des8. c16 |
  f2( aes4) aes |
  aes8 aes aes aes g4 g8. g16 |
  aes2. \bar"|."
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
  aes4 |
  aes aes aes |
  aes g g |
  g aes g |
  aes aes aes |
  aes aes aes |
  aes4. aes8 aes4 |
  aes bes bes |
  g2 g4 |
  aes aes aes |
  
  aes g g |
  g aes g |
  aes aes aes |
  aes aes aes |
  aes aes c |
  des ees des |
  c2 \bar"||" \time 4/4 \partial 4 s4 |
  s4 aes8. aes16 aes4 s |
  s aes8. aes16 aes4 aes8[ bes] |
  
  c c c c c4 c8 c |
  ees4 ees8. ees16 ees4 s |
  s aes,8. aes16 aes4 s |
  s aes8. aes16 des4 des |
  c8 c c c bes4 ees8. ees16 |
  c2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  aes,4 |
  aes aes aes |
  ees' ees ees |
  ees ees ees |
  aes, aes c |
  des des des |
  aes4. c8 ees4 |
  aes g f |
  ees2 ees4 |
  aes, aes aes |
  
  ees' ees ees |
  ees ees ees |
  aes, aes c |
  des des des |
  ees ees ees |
  ees ees ees |
  aes,2 \bar"||" \time 4/4 \partial 4 d4\rest |
  d\rest aes'8. aes16 aes4 d,\rest |
  d\rest aes'8. aes16 aes4 ees |
  
  f8 f f f f4 f8 f |
  ees4 ees8. ees16 ees4 d\rest |
  d\rest c8. ees16 aes4 d,\rest |
  d\rest des8. des16 des4 des |
  ees8 ees ees ees ees4 ees8. ees16 |
  aes,2. \bar"|."
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
    \new Lyrics = "altos"
    \new Lyrics = "altosII"
    \new Lyrics = "altosIII"
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \context Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \context Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Where There’s a Will There’s a Way"}}
  composer = \markup\oldStyleNum"Charles Edward Pollock"
  tagline = ""
}}


