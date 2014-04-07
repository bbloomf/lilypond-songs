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
  first-page-number = #112
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
  \key bes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	bes'4 g8 bes a[ g] f[ a] |
  g4 f b2\rest |
  bes4 g8 g a[ bes] c[ a] |
  bes2 bes\rest |
  
  bes4 g8[ bes] a[ g] f[ a] |
  g4 f b2\rest |
  bes4 g8[ g] a8[ bes] c[ a] |
  bes2 bes\rest |
  
  a4 a8 a a4 b8[ cis] |
  d4 a2 b4\rest |
  a4 f8[ f] e4 f8[ g] |
  a2. b4\rest |
  
  a4 a8[ a] a4 b8[ cis] |
  d4 a b2\rest |
  a4^\markup\italic"poco rall." b c g |
  c1\fermata |
  
  
	bes4 g8 bes a g f[ a] |
  g4 f b2\rest |
  bes4 g8 g a[ bes] c[ a] |
  bes2 bes\rest |
  
  bes4 g8 bes a[ g] f[ a] |
  g4 f b2\rest |
  bes4 g a8[ bes] c[ a] |
  bes2 bes\rest |
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	\set ignoreMelismata = ##t
  He was a lit -- tle tin _ sol -- dier,
  One lit -- tle leg __ _ had _ he;
  She was a lit -- tle fai -- ry danc -- er,
  Bright as __ _ bright _ could _ be.
  
  She had a cas -- tle and gar -- den,
  He but an old box __ _ dim;
  She was a dain -- ty __ _ \once \override LyricHyphen #'minimum-distance = #0.7 rose -- love,
  Far too grand for him.
  
  
  He was a lit -- tle tin _ sol -- dier,
  One lit -- tle leg _ had _ he;
  Brave -- ly he shoul -- _ dered his mus -- ket,
  Fain her love _ would _ be.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
	\set ignoreMelismata = ##t
  Once as he watch’d _ his _ \once \override LyricHyphen #'minimum-distance = #0.7 rose -- love,
  Winds from the north _ did _ blow,
  Swept him _ out _ of the case -- ment
  Down to a stream _ be -- _ low.
  
  True to his lit -- tle __ _ la -- dy,
  Still he __ _ shoul -- dered his gun;
  Soon, ah, __ _ soon came the dark -- ness,
  Life and love un -- done.
  
  
  He was a lit -- tle tin _ sol -- dier,
  One lit -- tle leg _ had _ he;
  Ne’er in the world _ a __ _ lov -- er
  Half so true _ could _ be.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
	\set ignoreMelismata = ##t
  Once more he sees _ his _ \once \override LyricHyphen #'minimum-distance = #0.7 rose -- love,
  Still she is danc -- _ ing _ gay,
  He is __ _ worn _ and _ fad -- ed,
  Loy -- al __ _ still _ for _ aye.
  
  Then came a hand that _ swept them,
  In -- to a fur -- nace __ _ wide,
  Part -- ed in life, in __ _ dy -- ing
  They are side by side.
  
  Ah! for the lit -- tle tin _ sol -- dier,
  Ah! for her cru -- _ el -- _ ty,
  There lies her rose _ in __ _ ash -- es,
  There his loy -- al lit -- tle heart.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4 d8 d ees[ ees] ees4 |
  d4 d s2 |
  d4 d8 d ees4 ees4 |
  d2 s |
  
  d4 d8[ d] ees8[ ees] ees[ ees] |
  d4 d s2 |
  d4 d8[ d] ees4 ees |
  d2 s |
  
  f4 f8 f g4 g8[ g] |
  f4 f2 s4 |
  f4 d8[ d] cis4 cis8[ cis] |
  f2. s4 |
  
  f4 f8[ f] g4 g8[ g] |
  f4 f s2 |
  f4 f f e |
  ees?1 |
  
  
  
  d4 d8 d ees8 ees8 ees4 |
  d4 d s2 |
  d4 d8 d ees4 ees |
  d2 s |
  
  d4 d8 d ees4 ees8[ ees] |
  d4 d s2 |
  d4 d ees8[ ees] ees[ ees] |
  d2 s |
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
  f,4 f8 f f[ f] a4 |
  f4 bes s2 |
  f4 f8 f f4 a |
  f2 s |
  
  f4 f8[ f] f[ f] a[ a] |
  f4 bes s2 |
  f4 f8[ f] f4 a |
  f2 s |
  
  a4 a8 a a4 a8[ a] |
  a4 a2 s4 |
  a a8[ a] g4 a8[ a] |
  a2. s4 |
  
  a4 a8[ a] a4 a8[ a] |
  a4 a s2 |
  a4 g bes bes |
  a1 |
  
  
  
  f4 f8 f f f a4 |
  f4 bes s2 |
  f4 f8 f f4 a |
  f2 s |
  
  f4 f8 f f4 a8[ a] |
  f4 bes s2 |
  f4 f f8[ f] a[ a] |
  f2 s \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  bes,4 bes8 bes c[ c] f4 |
  bes,4 bes d2\rest |
  bes4 bes8 bes c4 f |
  bes,2 d\rest |
  
  bes4 bes8[ bes] c[ c] f[ f] |
  bes,4 bes d2\rest |
  bes4 bes8[ bes] c4 f |
  bes,2 d\rest |
  
  d4 d8 d e4 a,8[ a] |
  d4 d2 d4\rest |
  d4 a8[ a] e'4 a,8[ a] |
  d2. d4\rest |
  
  d4 d8[ d] e4 a,8[ a] |
  d4 d d2\rest |
  d4 d c c |
  f1 |
  
  
  bes,4 bes8 bes c c f4 |
  bes,4 bes d2\rest |
  bes4 bes8 bes c4 f |
  bes,2 d\rest |
  
  bes4 bes8 bes c4 f8[ f] |
  bes,4 bes d2\rest |
  bes4 bes c8[ c] f[ f] |
  bes,2 d\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Little Tin Soldier"}}
  poet = \markup\oldStyleNum"Frederic Weatherly (1848–1929)"
  composer = \markup\oldStyleNum"James Lynam Molloy (1837–1909)"
  tagline = ""
}}


