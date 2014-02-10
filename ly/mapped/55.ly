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
       (padding . -8)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 200))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.875\in
  outer-margin = 0.375\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #55
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
  \time 3/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	b'4 b8\rest |
  b4 b8\rest |
  b8 a b |
  d[ c] a |
  a4 b8\rest |
  a4 b8\rest |
  a8 g a |
  b4 b8\rest |
  b4 b8\rest |
  b4 b8\rest |
  b8 a b |
  
  d[ c] a |
  a g a |
  d c a |
  g4 b8\rest |
  e4 b8\rest |
  d4 b8\rest |
  fis'4 b,8\rest |
  g'4 b,8\rest |
  a8 g a |
  d c a |
  g4 b8\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Thou, thou reign’st in this bos -- om,
  Here, here hast thou thy throne;
  Thou, thou know’st that I love thee,
  Am I not fond -- ly thine own?
  
  Yes, yes, yes, yes,
  Am I not fond -- ly thine own?
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Then, then, e’en as I love thee,
  Say, say, wilt thou love me?
  Thoughts, thoughts, ten -- der and true, love,
  Say wilt thou cher -- ish for me?
  
  Yes, yes, yes, yes,
  Say wilt thou cher -- ish for me?
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Speak, speak, love, I im -- plore thee;
  Say, say, hope shall be mine;
  Thou, thou, know’st that I love thee,
  Say but that thou wilt be mine;
  
  Yes, yes, yes, yes,
  Say but that thou wilt be mine.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'4 s8 |
  g4 s8 |
  g8 d g |
  b[ a] d, |
  d4 s8 |
  d4 s8 |
  d8 d fis |
  g4 s8 |
  g4 s8 |
  g4 s8 |
  g d g |
  
  b[ a] d, |
  fis e fis |
  g d fis |
  g4 s8 |
  c4 s8 |
  b4 s8 |
  a4 s8 |
  g4 s8 |
  fis8 e fis |
  fis d fis |
  g4 s8 \bar"|."
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
  d4 s8 |
  d4 s8 |
  d d d |
  d4 fis,8 |
  fis4 s8 |
  fis4 s8 |
  fis e fis |
  g4 s8 |
  d'4 s8 |
  d4 s8 |
  d d d |
  
  d4 fis,8 |
  d' d d |
  b a c |
  b4 s8 |
  c4 s8 |
  d4 s8 |
  c4 s8 |
  b4 s8 |
  d d d |
  a fis c' |
  b4 s8 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g4 d8\rest |
  g4 d8\rest |
  g fis g |
  fis4 d8 |
  d4 d8\rest |
  d4 d8\rest |
  d d d |
  g,4 d'8\rest |
  g4 d8\rest |
  g4 d8\rest |
  g fis g |
  
  fis4 d8 |
  d d d |
  d d d |
  g,4 d'8\rest |
  c4 d8\rest |
  g4 d8\rest |
  d4 d8\rest |
  e4 d8\rest |
  d8 d d |
  d d d |
  g4 d8\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Am I Not Fondly Thine Own"}}
  composer = \markup\oldStyleNum"German Folk Song"
  tagline = ""
}}


global = {
  \key d \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	d'2 d4 d |
  e2 e |
  fis4 d d e |
  d2 cis |
  fis fis4 fis |
  fis( e) e2 |
  d4. d8 e4 d |
  
  d2 cis |
  fis e4 d |
  g( fis) e2 |
  fis4. fis8 e4 d |
  cis( b) a2 |
  fis' d4 e |
  d2( cis) |
  d1 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	In -- te -- ger vi -- tæ scel -- e -- ris -- que pu -- rus
  Non e -- get Mau -- ris ja -- cu -- lis, nec ar -- cu,
  Nec ve -- ne -- na -- tis gra -- vi -- da sa -- git -- tis,
  Fus -- ce, pha -- re -- tra.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Si -- ve per Syr -- tes i -- ter æs -- tu -- o -- sas,
  Si -- ve fac -- tu -- rus per in -- hos -- pi -- ta -- lem
  Cau -- ca -- sum, vel quæ lo -- ca fa -- bu -- lo -- sus
  Lam -- bit Hy -- da -- spes.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Nam -- que me sil -- va lu -- pus in Sa -- bi -- na,
  Dum me -- am can -- to La -- la -- gen et ul -- tra
  Ter -- mi -- num cu -- ris va -- gor ex -- pe -- di -- tis,
  Fu -- git in -- er -- mem,
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
Qua -- le por -- ten -- tum ne -- que mi -- li -- ta -- ris
Dau -- ni -- as la -- tis a -- lit æs -- cu -- le -- tis
Nec Ju -- bæ tel -- lus ge -- ne -- rat, le -- o -- num
A -- ri -- da nu -- trix.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
Po -- ne me pi -- gris u -- bi nul -- la cam -- pis
Ar -- bor æ -- sti -- va re -- cre -- a -- tur au -- ra,
Quod la -- tus mun -- di ne -- bu -- læ ma -- lus -- que
Jup -- pi -- ter ur -- "get ;"
}

sopWordsVI = \lyricmode {
  \set stanza = #"6. "  
Po -- ne sub cur -- ru ni -- mi -- um pro -- pin -- qui
So -- lis in ter -- ra do -- mi -- bus ne -- ga -- "ta :"
Dul -- ce ri -- den -- tem La -- la -- gen a -- ma -- bo,
Dul -- ce lo -- quen -- tem.
}

altoMusic = \relative c' {
  a'2 a4 a |
  a2 a |
  a4 ais b b |
  a?2 a |
  a a4 a |
  ais2 ais |
  b4. b8 b4 b |
  
  b2 ais |
  d a?4 a |
  a2 a |
  a4. a8 gis4 gis |
  a( gis) a2 |
  a b4 b |
  a1 |
  a \bar "|."
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
  fis,2 fis4 fis |
  g2 g |
  fis4 fis g g |
  fis2 e |
  d d4 fis |
  fis2 fis |
  fis4. fis8 g4 fis |
  
  fis2 fis |
  a2 g4 fis |
  e2 e |
  d4. d8 e4 e |
  e( d) cis2 |
  d g4 g |
  fis2( e) |
  fis1 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  d,2 d4 d |
  cis2 cis |
  d4 d g, g |
  a2 a |
  d d4 d |
  cis2 cis |
  b4. b8 b4 b |
  
  fis2 fis |
  d'2 d4 d |
  cis2 cis |
  d4. d8 b4 b |
  a2 a |
  d2 g,4 e |
  a1 |
  d \bar"|."
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
    \new Lyrics = "altosVI"  \lyricsto "sopranos" \sopWordsVI
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Integer Vitæ"}}
  composer = \markup\oldStyleNum"Friedrich F. Flemming (1778–1813)"
  poet = \markup\oldStyleNum"Quintus Horatius Flaccus (65–8 BC)"
  tagline = ""
}}


